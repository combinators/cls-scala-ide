
package org.combinators.cls.ide.examples.guidemo

import java.io.InputStream
import java.nio.file.Paths

import com.github.javaparser.ast.CompilationUnit
import org.combinators.templating.persistable.{BundledResource, ResourcePersistable}
import org.combinators.cls.git.{InhabitationController, Results}
import org.combinators.templating.twirl.Java

object Helpers {
  //implicit val persistable: ResourcePersistable.Aux = ResourcePersistable.apply
  type Form = CompilationUnit
  type OptionSelection = Form => Runnable

  def addOptionSelection(form: Form, optionSelction: OptionSelection): Unit = {
    optionSelction(form).run
  }

  def addTitle(form: Form, title: String): Unit = {
    val cls = form.getClassByName("CustomerForm").get
    val initMethod = cls.getMethodsByName("initComponents").get(0)
    initMethod
      .getBody.get()
      .addStatement(0, Java(s"""this.setTitle("$title");""").statement())
  }

  def addLogo(form: Form, logoLocation: java.net.URL): Unit = {
    val cls = form.getClassByName("CustomerForm").get
    val initMethod = cls.getMethodsByName("initComponents").get(0)
    form.addImport("java.net.URL")
    initMethod
      .getBody.get()
      .addStatement(0, Java(
        s"""
           |try {
           |  this.add(new JLabel(new ImageIcon(new URL("$logoLocation"))));
           |} catch (Exception e) {
           |
           |}""".stripMargin).statement())
  }

  def readFile(name: String): String =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(name)).mkString

  /*def addDependencies(controller: InhabitationController)(results: Results): Results = {
    results.addExternalArtifact(BundledResource("build.sbt", Paths.get("build.sbt"), getClass))
  }*/
}
