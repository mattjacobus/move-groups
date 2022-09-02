package com.yurique.example
package renders

import scala.util.chaining.*
import scala.scalajs.js

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveSvgElement

object Page1Render {
  val colors = Seq(Seq("red", "blue"), Seq("green", "cyan"))

  def render(isIndex: Boolean): Render = page("Page 1") { () =>
    // Var[Seq[Seq[(color, index, opacity)]]]
    val colorsVar: Var[Seq[Seq[(String, Int, Double)]]] =
      Var(colors.map(_.zipWithIndex.map((c,i) => (c, i, 1.0))))

    val allRects$: Signal[Seq[(ReactiveSvgElement.Base, String)]] =
      colorsVar.signal.map(_.flatten).split(_._1) { case (_, (color, index, _), c$) =>
        val rect = svg.rect(
          svg.style <-- c$.map((_, _, opacity) => s"fill:$color;fill-opacity:$opacity"),
          svg.y(s"${index*100}"),
          svg.width("100"),
          svg.height("100")
        )
        (rect, color)
      }
    def filt(groupIndex: Int) =
      allRects$.combineWith(colorsVar.signal).map { (allRects, colors) =>
        allRects.filter((g, c) => colors(groupIndex).exists(_._1 == c)).map(_._1)
      }

    val aRects$ = filt(0)
    val bRects$ = filt(1)

    div(
      components.PageTitle("Page 1"),
      button(
        "Swap",
        styleAttr("border-radius:2em; background-color:#4eb5f1; color: white; padding:0.3em 1.2em;"),
        onClick --> { _ =>
          /* To make this work, put this update in two different transactions by
           *  commenting out this update() and uncommenting the two set()s below */
          colorsVar.update(_.reverse)
          //val colors = colorsVar.now()
          //colorsVar.set(Seq(Seq.empty, Seq.empty))
          //colorsVar.set(colors.reverse)
        }
      ),
      div(
        svg.svg(
          svg.width("400"),
          svg.height("400"),
          svg.g(
            children <-- aRects$
          ),
          svg.g(
            children <-- bRects$,
            svg.transform("translate(200)")
          ),
        ),
        onMountCallback { _ =>
          js.timers.setInterval(1000) {
            colorsVar.update(_.map(_.map((c, i, opacity) => (c, i, if opacity < 0.71 then 1.0 else 0.7))))
          }
        }
      )
    )
  }

}
