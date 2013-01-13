package SPOJ

object Liar { // #696
  // http://www.spoj.com/problems/LIAR/
  def solve(rows: Array[Array[Boolean]]) = {
    val indices = 0 to rows.length - 1

    def sat(fs: Seq[Int]) = {
      val ts = indices.diff(fs).toList
      ts forall { t => indices forall { i =>
        rows(t)(i) == ts.contains(i)
      }}
    }

    var lub = Int.MaxValue
    var glb = Int.MinValue

    for (n <- 0 to rows.length - rows.count(_.forall(!_))) {
      if (indices.combinations(n).exists(sat)) {
        if (n < lub) lub = n
        if (n > glb) glb = n
      }
    }

    if (lub == Int.MaxValue) None
    else Some(lub, glb)
  }

  object testCases {
    def A = Array

    val T = true
    val L = false

    val c1 = A(
      A(L, L),
      A(T, T)
    )

    val c1b = A(
      A(L, T),
      A(T, L)
    )

    val c1c = A(
      A(T, L),
      A(L, T)
    )

    val c3 = A(
      A(T, L, L, L),
      A(L, T, L, L),
      A(L, L, T, L),
      A(L, L, L, T)
    )

    val c4 = A(
      A(T, L, T, L, T),
      A(T, T, T, T, T),
      A(L, L, T, L, L),
      A(L, L, L, L, L),
      A(T, L, T, L, T)
    )
  }
}