package net.foggin.rules.plugin;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

object ColorManager {
  val XML_COMMENT = new RGB(128, 0, 0)
  val PROC_INSTR = new RGB(128, 128, 128)
  val STRING = new RGB(0, 128, 0)
  val DEFAULT = new RGB(0, 0, 0)
  val TAG = new RGB(0, 0, 128)
}

class ColorManager {

	protected val fColorTable = new scala.collection.mutable.HashMap[RGB, Color]

	def dispose() {
		for (colour <- fColorTable.values) colour.dispose
	}
	
	def getColor(rgb: RGB) = {
		fColorTable.getOrElseUpdate(rgb, new Color(Display.getCurrent(), rgb))
	}
}
