/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.nj2k.conversions

import org.jetbrains.kotlin.nj2k.NewJ2kConverterContext
import org.jetbrains.kotlin.nj2k.annotationByFqName
import org.jetbrains.kotlin.nj2k.tree.JKClass
import org.jetbrains.kotlin.nj2k.tree.JKOtherModifierElement
import org.jetbrains.kotlin.nj2k.tree.JKTreeElement
import org.jetbrains.kotlin.nj2k.tree.OtherModifier

private const val FUNCTIONAL_INTERFACE = "java.lang.FunctionalInterface"

internal class FunctionalInterfacesConverter(context: NewJ2kConverterContext) : RecursiveApplicableConversionBase(context) {
    override fun applyToElement(element: JKTreeElement): JKTreeElement {
        if (!context.functionalInterfaceConversionEnabled) return recurse(element)
        if (element !is JKClass) return recurse(element)
        if (element.classKind != JKClass.ClassKind.INTERFACE) return recurse(element)

        val functionalInterfaceMarker = element.annotationList.annotationByFqName(FUNCTIONAL_INTERFACE) ?: return recurse(element)

        element.otherModifierElements += JKOtherModifierElement(OtherModifier.FUN)
        element.annotationList.annotations -= functionalInterfaceMarker

        return recurse(element)
    }
}
