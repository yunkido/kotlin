/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.debugger.coroutine.proxy

import com.intellij.debugger.jdi.StackFrameProxyImpl

class SkipCoroutineStackFrameProxyImpl(val frame: StackFrameProxyImpl) :
    StackFrameProxyImpl(frame.threadProxy(), frame.stackFrame, frame.indexFromBottom) {
    override fun hashCode(): Int =
        frame.hashCode()

    override fun equals(other: Any?): Boolean =
        frame.equals(other)
}
