/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.codeInsight

import com.intellij.codeInsight.hints.*
import com.intellij.lang.Language
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiFile
import com.intellij.ui.layout.panel
import org.jetbrains.kotlin.idea.KotlinBundle
import org.jetbrains.kotlin.idea.KotlinLanguage
import javax.swing.JPanel

@Suppress("UnstableApiUsage")
class KotlinCodeVisionProvider : InlayHintsProvider<KotlinCodeVisionProvider.KotlinCodeVisionSettings> {

    override val key: SettingsKey<KotlinCodeVisionSettings> = SettingsKey("CodeLens")
    override val name: String = "Code Vision"
    override val previewText: String? = null

    override fun isLanguageSupported(language: Language): Boolean {
        return language is KotlinLanguage
    }

    override fun createConfigurable(settings: KotlinCodeVisionSettings): ImmediateConfigurable = object : ImmediateConfigurable {
        override fun createComponent(listener: ChangeListener): JPanel {
            return panel {}
        }

        override val cases: List<ImmediateConfigurable.Case>
            get() = listOf(
                ImmediateConfigurable.Case(
                    KotlinBundle.message("hints.title.codevision.usages"),
                    "usages",
                    settings::showUsages
                ),
                ImmediateConfigurable.Case(
                    KotlinBundle.message("hints.title.codevision.inheritors"),
                    "inheritors",
                    settings::showImplementations
                )
            )

        override val mainCheckboxText: String
            get() = KotlinBundle.message("hints.title.codevision.show.hints.for")
    }

    override fun createSettings(): KotlinCodeVisionSettings {
        return KotlinCodeVisionSettings()
    }

    override fun getCollectorFor(
        file: PsiFile,
        editor: Editor,
        settings: KotlinCodeVisionSettings,
        sink: InlayHintsSink
    ): InlayHintsCollector? {
        TODO("Not yet implemented")
    }

    data class KotlinCodeVisionSettings(var showUsages: Boolean = false, var showImplementations: Boolean = false)
}