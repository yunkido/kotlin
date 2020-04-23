// !LANGUAGE: +SuspendConversion

fun useSuspend(fn: suspend () -> Unit) {}

suspend fun foo0() {}
fun foo1() {}
fun foo2(vararg xs: Int) {}
fun foo3(): Int = 42
fun foo4(i: Int = 42) {}

class C {
    fun bar() {}
}

fun testLambda() { useSuspend { foo1() } }

fun testNoCoversion() { useSuspend(::foo0) }

fun testSimple() { useSuspend(::foo1) }

fun testWithVararg() { useSuspend(::foo2) }

fun testWithCoercionToUnit() { useSuspend(::foo3) }

fun testWithDefaults() { useSuspend(::foo4) }

fun testWithBoundReceiver() { useSuspend(C()::bar) }