package esmeta.es

/** ECMAScript script program */
case class Script(code: String, name: String, isBug: Boolean = false)
  extends ESElem

/** Script with minified tag */
case class ScriptM(
  code: String,
  name: String,
  minified: Option[Boolean] = None,
) extends ESElem

object ScriptConversions {

  // Implicit conversion from Script to ScriptM
  implicit def scriptToScriptM(script: Script): ScriptM =
    ScriptM(script.code, script.name)

  // Implicit conversion from ScriptM to Script
  implicit def scriptMToScript(scriptM: ScriptM): Script =
    Script(scriptM.code, scriptM.name)
}
