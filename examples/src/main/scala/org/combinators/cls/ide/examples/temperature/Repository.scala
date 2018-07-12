
package org.combinators.cls.ide.examples.temperature

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.{Type => JType}
import com.github.javaparser.ast.expr.Expression
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import org.combinators.cls.types.Type

trait Repository extends SemanticTypes {

  @combinator object Float {
    def apply : JType = Java("float").tpe()
    val semanticType:Type = precision(precision.floating)
  }

  @combinator object Integer {
    def apply : JType = Java("int").tpe()
    val semanticType:Type = precision(precision.integer)
  }

  // Truncation of Raw value might lose information
  @combinator object Truncate {
    def apply(exp:Expression) : Expression = Java(s"((int)$exp)").expression()
    val semanticType:Type = artifact(artifact.compute) :&: precision(precision.fullPrecision) :&: unit(unitType) =>:
      artifact(artifact.compute) :&: precision(precision.lossyPrecision :&: precision.integer) :&: unit(unitType)
  }

  @combinator object Worcester {
    def apply: String = "01609"
    val semanticType:Type = artifact(artifact.location)
  }

  // Offers static API for extracting current temperature as celsius
  @combinator object CurrentWeather {
    def apply(zip:String): CompilationUnit =
      Java(s"""|import java.io.*;
               |import java.net.*;
               |public class WeatherAPI {
               |  public static float getTemperature() {
               |		try {
               |			URL url = new URL("http://api.weatherunlocked.com/api/forecast/us.$zip?app_id={APPID}&app_key={APPKEY}");
               |			BufferedReader br = new BufferedReader(new InputStreamReader (url.openStream()));
               |			StringBuffer sb = new StringBuffer(br.readLine());
               |			int c = sb.indexOf("temp_c");
               |			return Float.valueOf(sb.substring(c+9, sb.indexOf(",", c)));
               |		} catch (Exception e) { return Float.NaN; }
               |	}
               |}""".stripMargin).compilationUnit()

    val semanticType:Type = artifact(artifact.location) =>:
      artifact(artifact.api) :&: precision(precision.floating :&: precision.fullPrecision) :&: unit(unit.celsius)
  }

  // Adapt some expression (precision/unit) without losing the (precision/unit)
  @combinator object AdaptInterface {
    def apply(temp:Expression, precision:JType): CompilationUnit = Java(s"""|public class TemperatureAdapter {
                                                                            |  $precision getTemperature() {
                                                                            |    return $temp;
                                                                            |  }
                                                                            |}""".stripMargin).compilationUnit()

    val semanticType:Type = artifact(artifact.compute) :&: precision(precisionType) :&: unit(unitType) =>:
      precision(precisionType) =>:
      artifact(artifact.impl) :&: precision(precisionType) :&: unit(unitType)
  }
  @combinator object CelsiusToFahrenheit {
    def apply(cels:Expression):Expression = Java(s"((9.0f/5)*$cels + 32)").expression()
    val semanticType:Type = artifact(artifact.compute) :&: precision(precision.fullPrecision) :&: unit(unit.celsius) =>:
      artifact(artifact.compute) :&: precision(precision.floating :&: precision.fullPrecision) :&: unit(unit.fahrenheit)
  }
  @combinator object CelsiusToKelvin {
    def apply(cels:Expression):Expression = Java(s"($cels + 273.15f)").expression()
    val semanticType:Type = artifact(artifact.compute) :&: precision(precision.fullPrecision) :&: unit(unit.celsius) =>:
      artifact(artifact.compute) :&: precision(precision.floating :&: precision.fullPrecision) :&: unit(unit.kelvin)
  }
  @combinator object TemperatureAPI {
    def apply:Expression = Java("WeatherAPI.getTemperature()").expression()
    val semanticType:Type = artifact(artifact.compute) :&: precision(precision.floating :&: precision.fullPrecision) :&: unit(unit.celsius)
  }
}


