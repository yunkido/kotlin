/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.inspections

import com.intellij.codeInspection.*
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.kotlin.idea.KotlinBundle
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameUnsafe

class ConvertNaNEqualityInspection : ResolveAbstractKotlinInspection() {
    override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean, session: LocalInspectionToolSession): PsiElementVisitor {
        return binaryExpressionVisitor { expression ->
            val resolver = session.resolver()
            if (expression.left.isNaNExpression(resolver) || expression.right.isNaNExpression(resolver)) {
                val inverted = when (expression.operationToken) {
                    KtTokens.EXCLEQ -> true
                    KtTokens.EQEQ -> false
                    else -> return@binaryExpressionVisitor
                }
                holder.registerProblem(
                    expression,
                    KotlinBundle.message("equality.check.with.nan.should.be.replaced.with.isnan"),
                    ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
                    ConvertNaNEqualityQuickFix(inverted, resolver)
                )
            }
        }
    }
}

private class ConvertNaNEqualityQuickFix(val inverted: Boolean, val resolver: KtElementAnalyzer) : LocalQuickFix {
    override fun getName() = KotlinBundle.message("convert.na.n.equality.quick.fix.text")

    override fun getFamilyName() = name

    override fun applyFix(project: Project, descriptor: ProblemDescriptor) {
        val element = descriptor.psiElement as? KtBinaryExpression ?: return

        val other = when {
            element.left.isNaNExpression(resolver) -> element.right ?: return
            element.right.isNaNExpression(resolver) -> element.left ?: return
            else -> return
        }
        val pattern = if (inverted) "!$0.isNaN()" else "$0.isNaN()"
        element.replace(KtPsiFactory(element).createExpressionByPattern(pattern, other))
    }
}

private val NaNSet = setOf("kotlin.Double.Companion.NaN", "java.lang.Double.NaN", "kotlin.Float.Companion.NaN", "java.lang.Float.NaN")

private fun KtExpression?.isNaNExpression(resolver: KtElementAnalyzer): Boolean {
    if (this?.text?.endsWith("NaN") != true) return false
    val fqName = resolver.resolveToCall(this)?.resultingDescriptor?.fqNameUnsafe?.asString()
    return NaNSet.contains(fqName)
}
