package com.yurique.example
package renders

import scala.util.chaining.*
import scala.scalajs.js

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveSvgElement

object Page1Render {
  val colors = Seq(Seq("red", "blue"), Seq("green", "cyan"))

  def render(isIndex: Boolean): Render = page("Page 1") { () =>
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
        onClick --> (_ => colorsVar.update(_.reverse))
      ),
      div(
        svg.svg(
          svg.width("400"),
          svg.height("400"),
          /* To make this work, but without the translate, uncomment this group and 
           *  comment out the two svg.g's below */
          // svg.g(
          //  children <-- aRects$.combineWith(bRects$).map(_ ++ _)
          //),
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
