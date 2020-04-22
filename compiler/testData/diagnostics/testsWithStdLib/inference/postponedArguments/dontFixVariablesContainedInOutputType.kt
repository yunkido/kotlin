// FIR_IDENTICAL
interface ObservableSet<out T> : Set<T> {}

fun <K> test(x: List<ObservableSet<K>>) {
    /*
     * Before the fix, we fix both variables `S` and `T` (see `reduce` declaration) before analysis lambda, to `ObservableSet<K>`.
     * It leads to error after analysis lambda as the return type actually became `Set<K>`.
     */
    x.reduce { acc: Set<K>, set: Set<K> -> acc + set }
}