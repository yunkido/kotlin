// !DIAGNOSTICS: -UNUSED_VARIABLE -UNUSED_EXPRESSION -UNCHECKED_CAST -UNUSED_PARAMETER -UNUSED_ANONYMOUS_PARAMETER

fun <T> materialize(): T = null as T

class Foo<out A> {
    fun <B> product(other: Foo<(A) -> B>) = materialize<Foo<B>>()

    fun <B, C, D, R> foo(other1: Foo<B>, other2: Foo<C>, other3: Foo<D>, function: (A, B, C, D) -> R) {
        val x = product<R>(
            other1.product(
                other2.product(
                    other3.product(
                        bar { d -> { c -> { b -> { a -> function(a, b, c, d) } } } }
                    )
                )
            )
        )
        <!DEBUG_INFO_EXPRESSION_TYPE("Foo<R>")!>x<!>
    }

    companion object {
        fun <A> bar(x: A) = materialize<Foo<A>>()
    }
}
