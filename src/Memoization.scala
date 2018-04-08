case class Memoization() {

  def memoizeFn[K, V](fn: K => V): K => V = {
    var cache = Map.empty[K, V]
    k => {
      if (!cache.contains(k)) {
        cache = cache.updated(k, fn(k))
      }
      cache(k)
    }
  }
}
