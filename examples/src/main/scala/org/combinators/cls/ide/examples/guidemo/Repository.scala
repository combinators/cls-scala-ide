
package org.combinators.cls.ide.examples.guidemo

import java.net.URL

import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Kinding, Type, Variable}
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java

import HelpersGui._

class Repository {
  lazy val alpha = Variable("alpha")
  lazy val kinding: Kinding =
    Kinding(alpha)
      .addOption('DropDown).addOption('RadioButtons)



  @combinator object customerForm {
    def apply(title: String,
              logoLocation: URL,
              optionSelector: OptionSelection): Form = {
      val form = Java(readFile("CustomerForm.java")).compilationUnit()
      addOptionSelection(form, optionSelector)
      addTitle(form, title)
      addLogo(form, logoLocation)
      form
    }
    val semanticType: Type =
      'Title =>: 'Location('Logo) =>: 'ChoiceDialog(alpha) =>: 'OrderMenu(alpha)
  }



  @combinator object dropDownSelector {
    def apply(databaseLocation: URL): OptionSelection = {
      form => new Runnable() {
        def run() = {
          form.addImport("java.util.List")
          form.addImport("java.util.ArrayList")
          form.addImport("com.fasterxml.jackson.databind.ObjectMapper")


          val cls = form.getClassByName("CustomerForm").get
          val initMethod = cls.getMethodsByName("initComponents").get(0)
          val getOrders =
            Java(
              s"""
                 |ObjectMapper mapper = new ObjectMapper();
                 |List<String> options;
                 |try{
                 |  options = mapper.readValue(
                 |    new URL("$databaseLocation"),
                 |    mapper.getTypeFactory().constructCollectionType(List.class, String.class));
                 |} catch (Exception e) {
                 |  options = new ArrayList<>();
                 |  options.add("");
                 |  JOptionPane.showMessageDialog(this, String.format("Could not load options: %s", e.getMessage()));
                 |}
                 |JComboBox optionBox = new JComboBox(options.toArray(new String[0]));
                 |optionBox.setSelectedIndex(0);
                 |selectedOrder = options.get(0);
                 |optionBox.addActionListener(e -> { selectedOrder = (String)optionBox.getSelectedItem(); });
                 |this.add(optionBox);
               """.stripMargin).statements()

          getOrders.reverse.foreach(stmt => initMethod.getBody.get().addStatement(0, stmt))
        }
      }
    }
    val semanticType: Type =
      'Location('Database) =>: 'ChoiceDialog('DropDown)
  }
  @combinator object radioButtonSelector {
    def apply(databaseLocation: URL): OptionSelection = {
      form => new Runnable() {
        def run() = {
          form.addImport("java.util.List")
          form.addImport("java.util.ArrayList")
          form.addImport("com.fasterxml.jackson.databind.ObjectMapper")

          val cls = form.getClassByName("CustomerForm").get
          val initMethod = cls.getMethodsByName("initComponents").get(0)
          val getOrders =
            Java(
              s"""
                 |ObjectMapper mapper = new ObjectMapper();
                 |List<String> options;
                 |try{
                 |  options = mapper.readValue(
                 |    new URL("$databaseLocation"),
                 |    mapper.getTypeFactory().constructCollectionType(List.class, String.class));
                 |} catch (Exception e) {
                 |  options = new ArrayList<>();
                 |  options.add("");
                 |  JOptionPane.showMessageDialog(this, String.format("Could not load options: %s", e.getMessage()));
                 |}
                 |ButtonGroup group = new ButtonGroup();
                 |for (String option : options) {
                 |  JRadioButton optionButton = new JRadioButton(option);
                 |  optionButton.addActionListener(e -> { selectedOrder = option; });
                 |  group.add(optionButton);
                 |  this.add(optionButton);
                 |}
                 |selectedOrder = options.get(0);
                 """.stripMargin).statements()
          getOrders.reverse.foreach(stmt => initMethod.getBody.get().addStatement(0, stmt))
        }
      }
    }
    val semanticType: Type =
      'Location('Database) =>: 'ChoiceDialog('RadioButtons)
  }
  @combinator object companyTitle {
    def apply: String = "Types Inc. - A LS14 Company"
    val semanticType: Type = 'Title
  }
  @combinator object databaseLocation {
    def apply: URL =
      new URL("http://localhost:9000/guidemo/productoptions")
    val semanticType: Type = 'Location('Database)
  }
  @combinator object logoLocation {
    def apply: URL =
      new URL("http://localhost:9000/guidemo/logo.png")
    val semanticType: Type  = 'Location('Logo)

  }
  @combinator object alternateLogoLocation {
    def apply: URL =
      new URL("http://localhost:9000/guidemo/alternatelogo.png")
    val semanticType: Type  = 'Location('Logo)
  }


}
