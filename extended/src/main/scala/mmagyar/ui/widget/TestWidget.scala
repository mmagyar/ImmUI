package mmagyar.ui.widget

import mmagyar.ihbp.{MacroTest, Widget}
import mmagyar.ui.core.{ElementList, Text}
import mmagyar.ui.interaction.{Behaviour, BehaviourBasic}
import mmagyar.ui.widget.base.WidgetBase

/** Created by Magyar Máté on 2017-05-25, All rights reserved. */

//
//trait A
//trait B
//@MacroTest
//class Test(val a:String) extends A with B { def getA:String = a}
//
////Gets transformed to by Intellij:
//class Test(val a: String) extends A() with B

//@Widget
//class TestWidget(val a:String)extends WidgetBase with DynamicGroupBasedWidgetBase[TestWidget]   {
//  override def generateElements: ElementList = ElementList(Text(a))
//  val behaviour: Behaviour[TestWidget] = BehaviourBasic()
//}
//
//object tt{
//  val a = new TestWidget("hai")
//
//  a.copy()setElements(ElementList(Text("NOO")))
//}