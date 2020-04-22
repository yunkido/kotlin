/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.inspections

import com.intellij.codeInspection.LocalInspectionTool
import com.intellij.codeInspection.LocalInspectionToolSession
import com.intellij.codeInspection.ProblemsHolder
import com.intellij.openapi.util.Key
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.kotlin.caches.resolve.KotlinCacheService
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.descriptors.VariableDescriptor
import org.jetbrains.kotlin.idea.caches.resolve.analyze
import org.jetbrains.kotlin.idea.caches.resolve.resolveToDescriptorIfAny
import org.jetbrains.kotlin.idea.util.logger
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.calls.callUtil.getResolvedCall
import org.jetbrains.kotlin.resolve.lazy.BodyResolveMode
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

abstract class ResolveAbstractKotlinInspection : AbstractKotlinInspection() {
    companion object {
        val KEY = Key<KtElementCacheAnalyzer>("LocalInspectionToolSessionResolver")
        val log by logger
    }

    override fun inspectionStarted(session: LocalInspectionToolSession, isOnTheFly: Boolean) {
        super.inspectionStarted(session, isOnTheFly)
        session.resolver().inspectionStarted(this)
    }

    override fun inspectionFinished(session: LocalInspectionToolSession, problemsHolder: ProblemsHolder) {
        super.inspectionFinished(session, problemsHolder)
        session.resolver().inspectionFinished(this)
    }

    abstract override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean, session: LocalInspectionToolSession): PsiElementVisitor
}

fun LocalInspectionToolSession.resolver(): KtElementAnalyzer =
    putUserDataIfAbsent(ResolveAbstractKotlinInspection.KEY, KtElementCacheAnalyzer(this))

interface KtElementAnalyzer {
    fun analyze(element: KtElement, resolveMode: BodyResolveMode = BodyResolveMode.PARTIAL): BindingContext

    fun analyzeFull(element: KtElement) =
        analyze(element, BodyResolveMode.FULL)

    fun analyzeWithCfa(element: KtElement) =
        analyze(element, BodyResolveMode.PARTIAL_WITH_CFA)

    fun getCallableDescriptor(element: KtExpression) =
        resolveToCall(element)?.resultingDescriptor

    fun resolveToCall(element: KtElement) =
        analyze(element).let {
            element.getResolvedCall(it)
        }

    fun resolveToVariableDescriptor(element: KtProperty): VariableDescriptor? =
        analyze(element).let { element.resolveToDescriptorIfAny(it) }

    fun resolveToDeclarationDescriptor(element: KtDeclaration): DeclarationDescriptor? =
        analyze(element).let { element.resolveToDescriptorIfAny(it) }

    fun resolveToClassDescriptor(element: KtClassOrObject): ClassDescriptor? =
        analyze(element).let { element.resolveToDescriptorIfAny(it) }

    fun resolveToDeclaration(element: KtDeclaration): DeclarationDescriptor? =
        analyze(element).let { element.resolveToDescriptorIfAny(it) }

    fun resolveToDescriptorIfAny(element: KtParameter) =
        analyze(element).let { element.resolveToDescriptorIfAny(it) }

    fun resolveToDescriptorIfAny(element: KtDeclaration): DeclarationDescriptor? =
        analyze(element).let { element.resolveToDescriptorIfAny(it) }

    fun inspectionFinished(inspectionTool: LocalInspectionTool) {}

    fun inspectionStarted(inspectionTool: LocalInspectionTool) {}

    companion object {
        val default = DefaultKtElementAnalyzer()
    }
}

class DefaultKtElementAnalyzer : KtElementAnalyzer {
    override fun analyze(element: KtElement, resolveMode: BodyResolveMode): BindingContext {
        return element.analyze(resolveMode)
    }
}

class KtElementCacheAnalyzer(val session: LocalInspectionToolSession) : KtElementAnalyzer {
    companion object {
        val log by logger
    }

    private val kotlinCacheService: KotlinCacheService by lazy { KotlinCacheService.getInstance(session.file.project) }
    private val sessionStoreResolve: ConcurrentHashMap<KtElement, BindingContextResolveMode> by lazy {
        log.debug("KtElementCacheAnalyzer created.")
        ConcurrentHashMap<KtElement, BindingContextResolveMode>()
    }
    private val counter = AtomicLong()
    private val missCounter = AtomicLong()
    private val upgradeCounter = AtomicLong()

    override fun analyze(element: KtElement, resolveMode: BodyResolveMode): BindingContext {
        counter.incrementAndGet()
        val resolveResult = sessionStoreResolve.computeIfAbsent(element) {
            BindingContextResolveMode()
        }
        return resolveResult.result(resolveMode, upgradeCounter) {
            val facade = kotlinCacheService.getResolutionFacade(listOf(element))
            missCounter.incrementAndGet()
            element.analyze(facade, resolveMode)
        }
    }

    override fun inspectionFinished(inspectionTool: LocalInspectionTool) {
        if (log.isDebugEnabled)
            log.debug("Inspection session finished: $session $inspectionTool ${counter}/${missCounter}")
    }

    override fun inspectionStarted(inspectionTool: LocalInspectionTool) {
        if (log.isDebugEnabled)
            log.debug("Inspection started on session $session $inspectionTool")
    }
}

class BindingContextResolveMode {
    private val resolveResults: Array<BindingContext?> = Array(DEFAULT_SIZE) { null }

    fun result(mode: BodyResolveMode, upgradeCounter: AtomicLong, resolver: (mode: BodyResolveMode) -> BindingContext): BindingContext {
        for (indexedValue in resolveResults.withIndex()) {
            val bc = indexedValue.value
            if (bc != null && arrayOfResolveMode[indexedValue.index].doesNotLessThan(mode)) {
                if (arrayOfResolveMode[indexedValue.index] != mode)
                    upgradeCounter.incrementAndGet()
                return bc
            }
        }
        return putInResult(mode, resolver)
    }

    private fun putInResult(mode: BodyResolveMode, bindingContextResolver: (mode: BodyResolveMode) -> BindingContext): BindingContext {
        val bindingContext = bindingContextResolver.invoke(mode)
        resolveResults[mode.ordinal] = bindingContext
        return bindingContext
    }

    companion object {
        val arrayOfResolveMode = BodyResolveMode.values()
        val DEFAULT_SIZE = BodyResolveMode.values().size
    }
}