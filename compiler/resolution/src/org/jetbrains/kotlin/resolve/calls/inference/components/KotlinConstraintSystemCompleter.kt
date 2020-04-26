/*
 * Copyright 2000-2018 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.resolve.calls.inference.components

import org.jetbrains.kotlin.builtins.*
import org.jetbrains.kotlin.descriptors.annotations.Annotations
import org.jetbrains.kotlin.resolve.calls.components.transformToResolvedLambda
import org.jetbrains.kotlin.resolve.calls.inference.NewConstraintSystem
import org.jetbrains.kotlin.resolve.calls.inference.model.*
import org.jetbrains.kotlin.resolve.calls.model.*
import org.jetbrains.kotlin.types.*
import org.jetbrains.kotlin.types.model.KotlinTypeMarker
import org.jetbrains.kotlin.types.model.TypeConstructorMarker
import org.jetbrains.kotlin.types.model.TypeVariableMarker
import org.jetbrains.kotlin.types.typeUtil.asTypeProjection
import org.jetbrains.kotlin.types.typeUtil.builtIns
import org.jetbrains.kotlin.types.typeUtil.supertypes
import org.jetbrains.kotlin.utils.addIfNotNull
import org.jetbrains.kotlin.utils.addToStdlib.safeAs
import kotlin.collections.LinkedHashSet

class KotlinConstraintSystemCompleter(
    private val resultTypeResolver: ResultTypeResolver,
    val variableFixationFinder: VariableFixationFinder,
) {
    enum class ConstraintSystemCompletionMode {
        FULL,
        PARTIAL
    }

    interface Context : VariableFixationFinder.Context, ResultTypeResolver.Context {
        val allTypeVariables: Map<TypeConstructorMarker, TypeVariableMarker>
        override val notFixedTypeVariables: Map<TypeConstructorMarker, VariableWithConstraints>
        override val postponedTypeVariables: List<TypeVariableMarker>

        // type can be proper if it not contains not fixed type variables
        fun canBeProper(type: KotlinTypeMarker): Boolean

        fun containsOnlyFixedOrPostponedVariables(type: KotlinTypeMarker): Boolean

        // mutable operations
        fun addError(error: KotlinCallDiagnostic)

        fun fixVariable(variable: TypeVariableMarker, resultType: KotlinTypeMarker, atom: ResolvedAtom?)
    }

    data class ParameterTypesInfo(
        val parametersFromDeclaration: List<UnwrappedType?>?,
        val parametersFromConstraints: Set<List<TypeWithKind?>>?,
        val annotations: Annotations,
        val isSuspend: Boolean,
        val isNullable: Boolean
    )

    data class TypeWithKind(
        val type: KotlinType,
        val direction: ConstraintKind = ConstraintKind.UPPER
    )

    fun runCompletion(
        c: Context,
        completionMode: ConstraintSystemCompletionMode,
        topLevelAtoms: List<ResolvedAtom>,
        topLevelType: UnwrappedType,
        diagnosticsHolder: KotlinDiagnosticsHolder,
        analyze: (PostponedResolvedAtom) -> Unit
    ) {
        c.runCompletion(
            completionMode,
            topLevelAtoms,
            topLevelType,
            diagnosticsHolder,
            collectVariablesFromContext = false,
            analyze = analyze
        )
    }

    fun completeConstraintSystem(
        c: Context,
        topLevelType: UnwrappedType,
        topLevelAtoms: List<ResolvedAtom>,
        diagnosticsHolder: KotlinDiagnosticsHolder
    ) {
        c.runCompletion(
            ConstraintSystemCompletionMode.FULL,
            topLevelAtoms,
            topLevelType,
            diagnosticsHolder,
            collectVariablesFromContext = true,
        ) {
            error("Shouldn't be called in complete constraint system mode")
        }
    }

    private fun Context.fixIndependentVariables(
        postponedArguments: List<PostponedResolvedAtom>,
        topLevelAtoms: List<ResolvedAtom>,
        topLevelType: UnwrappedType,
        collectVariablesFromContext: Boolean
    ) {
        while (true) {
            val typeVariablesWithProperConstraints = getOrderedAllTypeVariables(collectVariablesFromContext, topLevelAtoms).filter {
                variableFixationFinder.isTypeVariableHasProperConstraint(this, it)
            }
            val variableForFixation = variableFixationFinder.findIndependentVariableForFixation(
                this, typeVariablesWithProperConstraints, postponedArguments, ConstraintSystemCompletionMode.FULL, topLevelType,
            ) ?: break

            fixVariable(this, notFixedTypeVariables.getValue(variableForFixation.variable), topLevelAtoms)
        }
    }

    private fun Context.runCompletion(
        completionMode: ConstraintSystemCompletionMode,
        topLevelAtoms: List<ResolvedAtom>,
        topLevelType: UnwrappedType,
        diagnosticsHolder: KotlinDiagnosticsHolder,
        collectVariablesFromContext: Boolean,
        analyze: (PostponedResolvedAtom) -> Unit
    ) {
        val postponedArguments = getOrderedNotAnalyzedPostponedArguments(topLevelAtoms)

        val dependencyProvider = TypeVariableDependencyInformationProvider(notFixedTypeVariables, postponedArguments, topLevelType, this)

        while (true) {
            val variableForFixation = getVariableReadyForFixation(
                completionMode, topLevelAtoms, topLevelType, collectVariablesFromContext, postponedArguments
            ) ?: break

            val variableConstructor = variableForFixation.variable
            val v = variableForFixation.variable
            val variableWithConstraints = notFixedTypeVariables.getValue(variableConstructor)
            val isIndependentVariable =
                variableFixationFinder.isIndependentVariable(this, variableConstructor, postponedArguments, topLevelType)

            if (!postponedArguments.any { it.expectedType?.contains { v1 -> v == v1.typeConstructor() } == true } && !postponedArguments.any { it is PostponedAtomWithRevisableExpectedType && it.revisedExpectedType?.contains { v1 -> v == v1.typeConstructor() } == true }) {
                if (variableForFixation.hasProperConstraint && isIndependentVariable) {
                    fixVariable(this, variableWithConstraints, topLevelAtoms)
                    continue
                }
            }
            break
        }

        while (true) {
            val postponedArguments = getOrderedNotAnalyzedPostponedArguments(topLevelAtoms)

            val isThereAnyReadyForFixationVariable = isThereAnyReadyForFixationVariable(
                completionMode, topLevelAtoms, topLevelType, collectVariablesFromContext, postponedArguments
            )

            // If there aren't any postponed arguments and ready for fixation variables, then completion isn't needed: nothing to do
            if (postponedArguments.isEmpty() && !isThereAnyReadyForFixationVariable)
                break

            val argumentWithFixedOrPostponedInputTypes = findPostponedArgumentWithFixedOrPostponedInputTypes(postponedArguments)

            if (argumentWithFixedOrPostponedInputTypes != null) {
                analyze(argumentWithFixedOrPostponedInputTypes)
                continue
            }

            val postponedArgumentsWithRevisableTypeVariableExpectedType = postponedArguments
                .filter { it.expectedType?.constructor is TypeVariableTypeConstructor }
                .filterIsInstance<PostponedAtomWithRevisableExpectedType>()

            // Stage 1: collect parameter types from constraints and lambda parameters' declaration
            collectParameterTypesAndBuildNewExpectedTypes(postponedArgumentsWithRevisableTypeVariableExpectedType, completionMode)

            if (completionMode == ConstraintSystemCompletionMode.FULL) {
                // Stage 2: fix variables for parameter types
                for (argument in postponedArguments) {
                    val expectedType =
                        argument.run { safeAs<PostponedAtomWithRevisableExpectedType>()?.revisedExpectedType ?: expectedType }

                    if (expectedType != null && expectedType.isBuiltinFunctionalTypeOrSubtype) {
                        fixVariablesForParameterTypes(expectedType, postponedArguments, argument.outputType, topLevelType)
                    }
                }

                // Stage 3: create atoms with revised expected types if needed
                for (argument in postponedArgumentsWithRevisableTypeVariableExpectedType) {
                    transformToAtomWithNewFunctionalExpectedType(argument, diagnosticsHolder)
                }
            }

            /*
             * We should get not analyzed postponed arguments again because they can be changed by the stage of fixation type variables for parameters,
             * namely, postponed arguments with type variable as expected type can be replaced with resolved postponed arguments with functional expected type.
             *
             * See `transformToAtomWithNewFunctionalExpectedType`
             */
            val revisedPostponedArguments = getOrderedNotAnalyzedPostponedArguments(topLevelAtoms)

            // Stage 4: analyze the next ready postponed argument and rerun stages if it has been analyzed
            if (analyzeNextReadyPostponedArgument(revisedPostponedArguments, completionMode, analyze))
                continue

            // Stage 5: fix type variables – fix if possible or report not enough information
            val wasFixedSomeVariable = fixVariablesOrReportNotEnoughInformation(
                completionMode, topLevelAtoms, topLevelType, collectVariablesFromContext, revisedPostponedArguments, diagnosticsHolder
            )
            if (wasFixedSomeVariable)
                continue

            // Stage 6: force analysis of remaining not analyzed postponed arguments and rerun stages if there are
            if (completionMode == ConstraintSystemCompletionMode.FULL) {
                if (analyzeRemainingNotAnalyzedPostponedArgument(revisedPostponedArguments, analyze))
                    continue
            }

            break
        }
    }

    private fun Context.extractParameterTypesInfo(argument: PostponedAtomWithRevisableExpectedType): ParameterTypesInfo? {
        val expectedType = argument.expectedType ?: return null

        if (expectedType.constructor !in notFixedTypeVariables)
            return null

        /*
         * We shouldn't look at constraints for anonymous functions as they have fully explicit declaration (that's enough)
         */
        val foundFunctionalTypesInConstraints = if (!isAnonymousFunction(argument)) {
            findFunctionalTypesInConstraints(notFixedTypeVariables.getValue(expectedType.constructor))
        } else null

        val parameterTypesFromDeclaration =
            if (argument is LambdaWithTypeVariableAsExpectedTypeAtom) argument.parameterTypesFromDeclaration else null

        val parameterTypesFromConstraints = foundFunctionalTypesInConstraints?.map { typeWithKind ->
            typeWithKind.type.arguments.dropLast(1).map {
                // We should use opposite kind as lambda's parameters are contravariant
                TypeWithKind(it.type, typeWithKind.direction.opposite())
            }
        }?.toSet()

        val annotations = foundFunctionalTypesInConstraints?.run {
            Annotations.create(map { it.type.annotations }.flatten())
        }

        return ParameterTypesInfo(
            parameterTypesFromDeclaration,
            parameterTypesFromConstraints,
            annotations ?: Annotations.EMPTY,
            !foundFunctionalTypesInConstraints.isNullOrEmpty() && foundFunctionalTypesInConstraints.any { it.type.isSuspendFunctionTypeOrSubtype },
            !foundFunctionalTypesInConstraints.isNullOrEmpty() && foundFunctionalTypesInConstraints.all { it.type.isMarkedNullable }
        )
    }

    private fun extractFunctionalType(functionalTypeOrSubtype: KotlinType): KotlinType? {
        if (functionalTypeOrSubtype.isBuiltinFunctionalType)
            return functionalTypeOrSubtype

        return functionalTypeOrSubtype.supertypes().find { it.isBuiltinFunctionalType }
    }

    private fun Context.findFunctionalTypesInConstraints(
        variable: VariableWithConstraints,
        typeVariablesSeen: Set<TypeVariableTypeConstructor> = setOf()
    ): List<TypeWithKind>? {
        val typeVariableTypeConstructor = variable.typeVariable.freshTypeConstructor() as? TypeVariableTypeConstructor ?: return null

        if (typeVariableTypeConstructor in typeVariablesSeen) return null

        return variable.constraints.mapNotNull { constraint ->
            val type = constraint.type as? KotlinType ?: return@mapNotNull null

            when {
                type.isBuiltinFunctionalTypeOrSubtype ->
                    listOf(TypeWithKind(extractFunctionalType(type) ?: return@mapNotNull null, constraint.kind))
                type.constructor in notFixedTypeVariables -> {
                    findFunctionalTypesInConstraints(
                        notFixedTypeVariables.getValue(constraint.type.constructor),
                        typeVariablesSeen + typeVariableTypeConstructor
                    )
                }
                else -> null
            }
        }.flatten()
    }

    private fun extractParameterTypesFromDeclaration(atom: ResolutionAtom) =
        when (atom) {
            is FunctionExpression -> {
                val receiverType = atom.receiverType

                if (receiverType != null) listOf(receiverType) + atom.parametersTypes else atom.parametersTypes.toList()
            }
            is LambdaKotlinCallArgument -> atom.parametersTypes?.toList()
            else -> null
        }

    private fun NewConstraintSystem.createTypeVariableForParameterType(
        argument: PostponedAtomWithRevisableExpectedType,
        index: Int
    ): NewTypeVariable? {
        val expectedType = argument.expectedType ?: return null

        return when (argument) {
            is LambdaWithTypeVariableAsExpectedTypeAtom ->
                TypeVariableForLambdaParameterType(argument.atom, index, expectedType.builtIns, "_RP${index + 1}")
            is PostponedCallableReferenceAtom ->
                TypeVariableForCallableReferenceParameterType(expectedType.builtIns, "_QP${index + 1}")
            else -> null
        }?.apply { getBuilder().registerVariable(this) }
    }

    private fun NewConstraintSystem.createTypeVariableForReturnType(argument: PostponedAtomWithRevisableExpectedType): NewTypeVariable? {
        val expectedType = argument.expectedType ?: return null

        return when (argument) {
            is LambdaWithTypeVariableAsExpectedTypeAtom -> TypeVariableForLambdaReturnType(expectedType.builtIns, "_R")
            is PostponedCallableReferenceAtom -> TypeVariableForCallableReferenceReturnType(expectedType.builtIns, "_Q")
            else -> null
        }?.apply { getBuilder().registerVariable(this) }
    }

    private fun NewConstraintSystem.createTypeVariablesForParameters(
        argument: PostponedAtomWithRevisableExpectedType,
        parameterTypes: List<List<TypeWithKind?>>
    ): List<TypeProjection>? {
        val atom = argument.atom
        val csBuilder = getBuilder()
        val allGroupedParameterTypes = parameterTypes.first().indices.map { i -> parameterTypes.map { it.getOrNull(i) } }

        return allGroupedParameterTypes.mapIndexedNotNull { index, types ->
            val parameterTypeVariable = createTypeVariableForParameterType(argument, index) ?: return@mapIndexedNotNull null

            for (typeWithKind in types.filterNotNull()) {
                when (typeWithKind.direction) {
                    ConstraintKind.EQUALITY ->
                        csBuilder.addEqualityConstraint(
                            parameterTypeVariable.defaultType, typeWithKind.type, ArgumentConstraintPosition(atom)
                        )
                    ConstraintKind.UPPER ->
                        csBuilder.addSubtypeConstraint(
                            parameterTypeVariable.defaultType, typeWithKind.type, ArgumentConstraintPosition(atom)
                        )
                    ConstraintKind.LOWER ->
                        csBuilder.addSubtypeConstraint(
                            typeWithKind.type, parameterTypeVariable.defaultType, ArgumentConstraintPosition(atom)
                        )
                }
            }

            parameterTypeVariable.defaultType.asTypeProjection()
        }
    }

    private fun Context.collectParameterTypesAndBuildNewExpectedTypes(
        postponedArguments: List<PostponedAtomWithRevisableExpectedType>,
        completionMode: ConstraintSystemCompletionMode
    ) {
        if (this !is NewConstraintSystem) return

        // We can collect parameter types from declaration in any mode, they can't change during completion.
        val postponedArgumentsToCollectTypesFromDeclaredParameters = postponedArguments
            .filterIsInstance<LambdaWithTypeVariableAsExpectedTypeAtom>()
            .filter { it.parameterTypesFromDeclaration == null }

        for (argument in postponedArgumentsToCollectTypesFromDeclaredParameters) {
            argument.parameterTypesFromDeclaration = extractParameterTypesFromDeclaration(argument.atom)
        }

        /*
         * We can build new functional expected types in partial mode only for anonymous functions,
         * because more exact type can't appear from constraints in full mode (anonymous functions have fully explicit declaration).
         * It can be so for lambdas: for instance, an extension function type can appear in full mode (it may not be known in partial mode).
         */
        val postponedArgumentsToCollectParameterTypesAndBuildNewExpectedType =
            if (completionMode == ConstraintSystemCompletionMode.PARTIAL) {
                postponedArguments.filter(::isAnonymousFunction)
            } else {
                postponedArguments
            }

        do {
            val wasTransformedSomePostponedArgument =
                postponedArgumentsToCollectParameterTypesAndBuildNewExpectedType.filter { it.revisedExpectedType == null }.any { argument ->
                    val parameterTypesInfo = extractParameterTypesInfo(argument) ?: return@any false
                    val revisedParameterTypesInfo =
                        takeIntoAccountOtherLambdasDeclaredParameters(argument, postponedArguments, parameterTypesInfo)
                    val newExpectedType = buildNewFunctionalExpectedType(argument, revisedParameterTypesInfo) ?: return@any false

                    argument.revisedExpectedType = newExpectedType

                    true
                }
        } while (wasTransformedSomePostponedArgument)
    }

    private fun Context.takeIntoAccountOtherLambdasDeclaredParameters(
        argument: PostponedAtomWithRevisableExpectedType,
        postponedArguments: List<PostponedAtomWithRevisableExpectedType>,
        parameterTypesInfo: ParameterTypesInfo
    ): ParameterTypesInfo {
        fun PostponedAtomWithRevisableExpectedType.getExpectedTypeConstructor() =
            expectedType?.typeConstructor() as? TypeVariableTypeConstructor

        val parameterTypesFromDeclarationOfRelatedLambdas = postponedArguments
            .filterIsInstance<LambdaWithTypeVariableAsExpectedTypeAtom>()
            .filter { it.parameterTypesFromDeclaration != null && it != argument }
            .mapNotNull { anotherArgument ->
                val argumentExpectedTypeConstructor = argument.getExpectedTypeConstructor() ?: return@mapNotNull null
                val anotherArgumentExpectedTypeConstructor = anotherArgument.getExpectedTypeConstructor() ?: return@mapNotNull null

                if (areTypeVariablesRelated(argumentExpectedTypeConstructor, anotherArgumentExpectedTypeConstructor)) {
                    anotherArgument.parameterTypesFromDeclaration?.map { it.wrapToTypeWithKind() }
                } else null
            }

        return if (parameterTypesFromDeclarationOfRelatedLambdas.isNotEmpty()) {
            val newParametersFromConstraints =
                parameterTypesInfo.parametersFromConstraints.orEmpty() + parameterTypesFromDeclarationOfRelatedLambdas.toSet()

            parameterTypesInfo.copy(parametersFromConstraints = newParametersFromConstraints)
        } else parameterTypesInfo
    }

    private fun Context.areTypeVariablesRelated(
        a: TypeVariableTypeConstructor,
        b: TypeVariableTypeConstructor,
        variablesSeen: Set<TypeVariableTypeConstructor> = setOf()
    ): Boolean {
        if (a == b) return true
        if (a !in notFixedTypeVariables || a in variablesSeen) return false

        return notFixedTypeVariables.getValue(a).constraints.any {
            val typeConstructor = it.type.typeConstructor()
            val variableTypeConstructor =
                notFixedTypeVariables[typeConstructor]?.typeVariable?.freshTypeConstructor() as? TypeVariableTypeConstructor

            typeConstructor == b ||
                    (variableTypeConstructor != null && areTypeVariablesRelated(variableTypeConstructor, b, variablesSeen + a))
        }
    }

    private fun NewConstraintSystem.buildNewFunctionalExpectedType(
        argument: PostponedAtomWithRevisableExpectedType,
        parameterTypesInfo: ParameterTypesInfo
    ): UnwrappedType? {
        val expectedTypeVariable = argument.expectedType ?: return null
        val atom = argument.atom
        val parametersFromConstraints = parameterTypesInfo.parametersFromConstraints
        val parametersFromDeclaration = prependReceiverTypeIfItIsExtensionFunction(parameterTypesInfo)
        val allParameterTypes =
            (parametersFromConstraints.orEmpty() + parametersFromDeclaration?.map { it.wrapToTypeWithKind() }).filterNotNull()

        if (allParameterTypes.isEmpty())
            return null

        val variablesForParameterTypes = createTypeVariablesForParameters(argument, allParameterTypes) ?: return null
        val returnValueVariable = createTypeVariableForReturnType(argument) ?: return null

        val functionDescriptor = when (argument) {
            is LambdaWithTypeVariableAsExpectedTypeAtom ->
                getFunctionDescriptor(expectedTypeVariable.builtIns, variablesForParameterTypes.size, parameterTypesInfo.isSuspend)
            is PostponedCallableReferenceAtom ->
                getKFunctionDescriptor(expectedTypeVariable.builtIns, variablesForParameterTypes.size, parameterTypesInfo.isSuspend)
            else -> null
        } ?: return null

        val isExtensionFunctionType = parameterTypesInfo.annotations.hasExtensionFunctionAnnotation()
        val areAllParameterTypesFromDeclarationSpecified =
            !parametersFromDeclaration.isNullOrEmpty() && parametersFromDeclaration.all { it != null }
        val areParametersNumberInDeclarationAndConstraintsEqual =
            !parametersFromDeclaration.isNullOrEmpty() && !parametersFromConstraints.isNullOrEmpty()
                    && parametersFromDeclaration.size == parametersFromConstraints.first().size
        /*
         * We need to exclude further considering a postponed argument as an extension function
         * to support cases with explicitly specified receiver as a value parameter (only if all parameter types are specified)
         *
         * Example: `val x: String.() -> Int = id { x: String -> 42 }`
         */
        val shouldDiscriminateExtensionFunctionAnnotation =
            isExtensionFunctionType && areAllParameterTypesFromDeclarationSpecified && areParametersNumberInDeclarationAndConstraintsEqual

        /*
         * We need to add an extension function annotation for anonymous functions with an explicitly specified receiver
         *
         * Example: `val x = id(fun String.() = this)`
         */
        val shouldAddExtensionFunctionAnnotation = atom is FunctionExpression && atom.receiverType != null

        val annotations = when {
            shouldDiscriminateExtensionFunctionAnnotation ->
                parameterTypesInfo.annotations.withoutExtensionFunctionAnnotation()
            shouldAddExtensionFunctionAnnotation ->
                parameterTypesInfo.annotations.withExtensionFunctionAnnotation(expectedTypeVariable.builtIns)
            else -> parameterTypesInfo.annotations
        }

        val nexExpectedType = KotlinTypeFactory.simpleType(
            annotations,
            functionDescriptor.typeConstructor,
            variablesForParameterTypes + returnValueVariable.defaultType.asTypeProjection(),
            parameterTypesInfo.isNullable
        )

        getBuilder().addSubtypeConstraint(
            nexExpectedType,
            expectedTypeVariable,
            ArgumentConstraintPosition(argument.atom)
        )

        return nexExpectedType
    }

    private fun Context.transformToAtomWithNewFunctionalExpectedType(
        argument: PostponedAtomWithRevisableExpectedType,
        diagnosticsHolder: KotlinDiagnosticsHolder
    ) {
        if (this !is NewConstraintSystem) return

        val revisedExpectedType = argument.revisedExpectedType ?: return

        when (argument) {
            is PostponedCallableReferenceAtom -> {
                PostponedCallableReferenceAtom(EagerCallableReferenceAtom(argument.atom, revisedExpectedType)).also {
                    argument.setAnalyzedResults(null, listOf(it))
                }
            }
            is LambdaWithTypeVariableAsExpectedTypeAtom -> {
                val returnTypeVariableConstructor = revisedExpectedType.arguments.last().type.constructor
                val returnTypeVariable =
                    notFixedTypeVariables.getValue(returnTypeVariableConstructor).typeVariable as? TypeVariableForLambdaReturnType ?: return

                argument.transformToResolvedLambda(getBuilder(), diagnosticsHolder, revisedExpectedType, returnTypeVariable)
            }
        }
    }

    private fun Context.fixVariablesInsideConstraints(
        variableWithConstraints: VariableWithConstraints,
        argumentOutputType: KotlinType?,
        topLevelAtoms: List<ResolvedAtom>,
        topLevelType: UnwrappedType,
        variablesSeen: Set<TypeVariableTypeConstructor>
    ) {
        mutableListOf<Constraint>().apply { addAll(variableWithConstraints.constraints) }.forEach {
            fixVariablesInsideType(it.type as KotlinType, topLevelAtoms, argumentOutputType, topLevelType, variablesSeen)
        }
    }

    private fun Context.fixVariablesInsideType(
        type: KotlinType,
        topLevelAtoms: List<ResolvedAtom>,
        argumentOutputType: KotlinType?,
        topLevelType: UnwrappedType,
        variablesSeen: Set<TypeVariableTypeConstructor> = setOf()
    ) {
        val typeConstructor = type.constructor

        if (typeConstructor in variablesSeen) return

        if (typeConstructor is TypeVariableTypeConstructor && typeConstructor in notFixedTypeVariables) {
            val variableWithConstraints = notFixedTypeVariables.getValue(typeConstructor)

//            fixVariablesInsideConstraints(variableWithConstraints, argumentOutputType, topLevelAtoms, topLevelType, variablesSeen + typeConstructor)

            if (variableFixationFinder.isTypeVariableHasProperConstraint(this, typeConstructor)) {
                val isPostponedVariable = variableWithConstraints.typeVariable in postponedTypeVariables
                val isContainedInOutputType = argumentOutputType?.contains { it.typeConstructor() == typeConstructor } == true

                /*
                 * We don't fix variables which is contained in the postponed argument's return type,
                 * as it can lead to errors after analysis (the obtained actual return type and corresponding type variable fixed before can have contradiction).
                 *
                 * But we take into account only the current postponed argument to support resolution dependent lambdas by input-output types:
                 *      fun <K, V> A<K>.toB(f: (V) -> K, g: (K) -> V): B<K, V> = B()
                 */
                if (!isContainedInOutputType && !isPostponedVariable && typeConstructor in notFixedTypeVariables) {
                    fixVariable(this, variableWithConstraints, topLevelAtoms)
                }
            }
        } else if (type.arguments.isNotEmpty()) {
            for (argument in type.arguments) {
                fixVariablesInsideType(argument.type, topLevelAtoms, argumentOutputType, topLevelType, variablesSeen)
            }
        }
    }

    private fun Context.fixVariablesForParameterTypes(
        type: KotlinType,
        topLevelAtoms: List<ResolvedAtom>,
        argumentOutputType: KotlinType?,
        topLevelType: UnwrappedType
    ) {
        for (parameter in type.arguments.dropLast(1)) {
            fixVariablesInsideType(parameter.type, topLevelAtoms, argumentOutputType, topLevelType)
        }
    }

    private fun prependReceiverTypeIfItIsExtensionFunction(parameterTypesInfo: ParameterTypesInfo): List<UnwrappedType?>? {
        val (parametersFromDeclaration, parametersFromConstraints, annotations) = parameterTypesInfo

        if (parametersFromConstraints.isNullOrEmpty() || parametersFromDeclaration.isNullOrEmpty())
            return parametersFromDeclaration

        val oneLessParameterInDeclarationThanInConstraints = parametersFromConstraints.first().size == parametersFromDeclaration.size + 1

        return if (oneLessParameterInDeclarationThanInConstraints && annotations.hasExtensionFunctionAnnotation()) {
            listOf(null) + parametersFromDeclaration
        } else {
            parametersFromDeclaration
        }
    }

    private fun Context.analyzeNextReadyPostponedArgument(
        postponedArguments: List<PostponedResolvedAtom>,
        completionMode: ConstraintSystemCompletionMode,
        analyze: (PostponedResolvedAtom) -> Unit
    ): Boolean {
        if (completionMode == ConstraintSystemCompletionMode.FULL) {
            val argumentWithTypeVariableAsExpectedType = findPostponedArgumentWithRevisableTypeVariableExpectedType(postponedArguments)

            if (argumentWithTypeVariableAsExpectedType != null) {
                analyze(argumentWithTypeVariableAsExpectedType)
                return true
            }
        }

        val argumentWithFixedOrPostponedInputTypes = findPostponedArgumentWithFixedOrPostponedInputTypes(postponedArguments)

        if (argumentWithFixedOrPostponedInputTypes != null) {
            analyze(argumentWithFixedOrPostponedInputTypes)
            return true
        }

        return false
    }

    private fun findPostponedArgumentWithRevisableTypeVariableExpectedType(postponedArguments: List<PostponedResolvedAtom>) =
        postponedArguments.firstOrNull { argument ->
            argument is PostponedAtomWithRevisableExpectedType && argument.expectedType?.constructor is TypeVariableTypeConstructor
        }

    private fun analyzeRemainingNotAnalyzedPostponedArgument(
        postponedArguments: List<PostponedResolvedAtom>,
        analyze: (PostponedResolvedAtom) -> Unit
    ): Boolean {
        val remainingNotAnalyzedPostponedArgument = postponedArguments.firstOrNull { !it.analyzed }

        if (remainingNotAnalyzedPostponedArgument != null) {
            analyze(remainingNotAnalyzedPostponedArgument)
            return true
        }

        return false
    }

    private fun Context.getVariableReadyForFixation(
        completionMode: ConstraintSystemCompletionMode,
        topLevelAtoms: List<ResolvedAtom>,
        topLevelType: UnwrappedType,
        collectVariablesFromContext: Boolean,
        postponedArguments: List<PostponedResolvedAtom>
    ) = variableFixationFinder.findFirstVariableForFixation(
        this,
        getOrderedAllTypeVariables(collectVariablesFromContext, topLevelAtoms),
        postponedArguments,
        completionMode,
        topLevelType
    )

    private fun Context.isThereAnyReadyForFixationVariable(
        completionMode: ConstraintSystemCompletionMode,
        topLevelAtoms: List<ResolvedAtom>,
        topLevelType: UnwrappedType,
        collectVariablesFromContext: Boolean,
        postponedArguments: List<PostponedResolvedAtom>
    ) = getVariableReadyForFixation(completionMode, topLevelAtoms, topLevelType, collectVariablesFromContext, postponedArguments) != null

    private fun Context.fixVariablesOrReportNotEnoughInformation(
        completionMode: ConstraintSystemCompletionMode,
        topLevelAtoms: List<ResolvedAtom>,
        topLevelType: UnwrappedType,
        collectVariablesFromContext: Boolean,
        postponedArguments: List<PostponedResolvedAtom>,
        diagnosticsHolder: KotlinDiagnosticsHolder
    ): Boolean {
        var wasFixedSomeVariable = false

        while (true) {
            val variableForFixation = getVariableReadyForFixation(
                completionMode,
                topLevelAtoms,
                topLevelType,
                collectVariablesFromContext,
                postponedArguments
            ) ?: break

            if (variableForFixation.hasProperConstraint || completionMode == ConstraintSystemCompletionMode.FULL) {
                val variableWithConstraints = notFixedTypeVariables.getValue(variableForFixation.variable)

                if (variableForFixation.hasProperConstraint) {
                    fixVariable(this, variableWithConstraints, topLevelAtoms)
                    wasFixedSomeVariable = true
                } else {
                    processVariableWhenNotEnoughInformation(this, variableWithConstraints, topLevelAtoms, diagnosticsHolder)
                }

                continue
            }

            break
        }

        return wasFixedSomeVariable
    }

    private fun Context.findPostponedArgumentWithFixedOrPostponedInputTypes(postponedArguments: List<PostponedResolvedAtom>) =
        postponedArguments.firstOrNull { argument ->
            argument.inputTypes.all { containsOnlyFixedOrPostponedVariables(it) }
        }

    private fun Context.getOrderedAllTypeVariables(
        collectVariablesFromContext: Boolean,
        topLevelAtoms: List<ResolvedAtom>
    ): List<TypeConstructorMarker> {
        if (collectVariablesFromContext)
            return notFixedTypeVariables.keys.toList()

        fun getVariablesFromRevisedExpectedType(revisedExpectedType: KotlinType?) =
            revisedExpectedType?.arguments?.map { it.type.constructor }?.filterIsInstance<TypeVariableTypeConstructor>()

        fun ResolvedAtom.process(to: LinkedHashSet<TypeConstructor>) {
            val typeVariables = when (this) {
                is LambdaWithTypeVariableAsExpectedTypeAtom -> getVariablesFromRevisedExpectedType(revisedExpectedType).orEmpty()
                is ResolvedCallAtom -> freshVariablesSubstitutor.freshVariables.map { it.freshTypeConstructor }
                is PostponedCallableReferenceAtom ->
                    getVariablesFromRevisedExpectedType(revisedExpectedType).orEmpty() +
                            candidate?.freshSubstitutor?.freshVariables?.map { it.freshTypeConstructor }.orEmpty()
                is ResolvedCallableReferenceAtom -> candidate?.freshSubstitutor?.freshVariables?.map { it.freshTypeConstructor }.orEmpty()
                is ResolvedLambdaAtom -> listOfNotNull(typeVariableForLambdaReturnType?.freshTypeConstructor)
                else -> emptyList()
            }

            typeVariables.mapNotNullTo(to) {
                it.takeIf { notFixedTypeVariables.containsKey(it) }
            }

            /*
             * Hack for completing error candidates in delegate resolve
             */
            if (this is StubResolvedAtom && typeVariable in notFixedTypeVariables) {
                to += typeVariable
            }

            if (analyzed) {
                subResolvedAtoms?.forEach { it.process(to) }
            }
        }

        // Note that it's important to use Set here, because several atoms can share the same type variable
        val result = linkedSetOf<TypeConstructor>()
        for (primitive in topLevelAtoms) {
            primitive.process(result)
        }

        assert(result.size == notFixedTypeVariables.size) {
            val notFoundTypeVariables = notFixedTypeVariables.keys.toMutableSet().apply { removeAll(result) }
            "Not all type variables found: $notFoundTypeVariables"
        }

        return result.toList()
    }

    private fun fixVariable(
        c: Context,
        variableWithConstraints: VariableWithConstraints,
        topLevelAtoms: List<ResolvedAtom>
    ) {
        fixVariable(c, variableWithConstraints, TypeVariableDirectionCalculator.ResolveDirection.UNKNOWN, topLevelAtoms)
    }

    private fun fixVariable(
        c: Context,
        variableWithConstraints: VariableWithConstraints,
        direction: TypeVariableDirectionCalculator.ResolveDirection,
        topLevelAtoms: List<ResolvedAtom>
    ) {
        val resultType = resultTypeResolver.findResultType(c, variableWithConstraints, direction)
        val resolvedAtom = findResolvedAtomBy(variableWithConstraints.typeVariable, topLevelAtoms) ?: topLevelAtoms.firstOrNull()
        c.fixVariable(variableWithConstraints.typeVariable, resultType, resolvedAtom)
    }

    private fun processVariableWhenNotEnoughInformation(
        c: Context,
        variableWithConstraints: VariableWithConstraints,
        topLevelAtoms: List<ResolvedAtom>,
        diagnosticsHolder: KotlinDiagnosticsHolder
    ) {
        val typeVariable = variableWithConstraints.typeVariable

        val resolvedAtom = findResolvedAtomBy(typeVariable, topLevelAtoms) ?: topLevelAtoms.firstOrNull()
        if (resolvedAtom != null) {
            c.addError(NotEnoughInformationForTypeParameter(typeVariable, resolvedAtom))
        }

        val resultErrorType = when {
            typeVariable is TypeVariableFromCallableDescriptor ->
                ErrorUtils.createUninferredParameterType(typeVariable.originalTypeParameter)
            typeVariable is TypeVariableForLambdaParameterType && typeVariable.atom is LambdaKotlinCallArgument -> {
                diagnosticsHolder.addDiagnostic(NotEnoughInformationForLambdaParameter(typeVariable.atom, typeVariable.index))
                ErrorUtils.createErrorType("Cannot infer lambda parameter type")
            }
            else -> ErrorUtils.createErrorType("Cannot infer type variable $typeVariable")
        }

        c.fixVariable(typeVariable, resultErrorType, resolvedAtom)
    }

    private fun findResolvedAtomBy(typeVariable: TypeVariableMarker, topLevelAtoms: List<ResolvedAtom>): ResolvedAtom? {
        fun ResolvedAtom.check(): ResolvedAtom? {
            val suitableCall = when (this) {
                is ResolvedCallAtom -> typeVariable in freshVariablesSubstitutor.freshVariables
                is ResolvedCallableReferenceAtom -> candidate?.freshSubstitutor?.freshVariables?.let { typeVariable in it } ?: false
                is ResolvedLambdaAtom -> typeVariable == typeVariableForLambdaReturnType
                else -> false
            }

            if (suitableCall) {
                return this
            }

            subResolvedAtoms?.forEach { subResolvedAtom ->
                subResolvedAtom.check()?.let { result -> return@check result }
            }

            return null
        }

        for (topLevelAtom in topLevelAtoms) {
            topLevelAtom.check()?.let { return it }
        }

        return null
    }

    private fun KotlinType?.wrapToTypeWithKind() = this?.let { TypeWithKind(it) }

    private fun isAnonymousFunction(argument: PostponedAtomWithRevisableExpectedType) = argument.atom is FunctionExpression

    companion object {
        fun getOrderedNotAnalyzedPostponedArguments(topLevelAtoms: List<ResolvedAtom>): List<PostponedResolvedAtom> {
            fun ResolvedAtom.process(to: MutableList<PostponedResolvedAtom>) {
                to.addIfNotNull(this.safeAs<PostponedResolvedAtom>()?.takeUnless { it.analyzed })

                if (analyzed) {
                    subResolvedAtoms?.forEach { it.process(to) }
                }
            }

            val notAnalyzedArguments = arrayListOf<PostponedResolvedAtom>()
            for (primitive in topLevelAtoms) {
                primitive.process(notAnalyzedArguments)
            }

            return notAnalyzedArguments
        }
    }
}
