import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import java.util.*;
import javax.tools.Diagnostic.Kind;

/** Check that a class marked {@code @Utility} is indeed a utility class. */
@SupportedAnnotationTypes("Utility")
@SupportedSourceVersion(SourceVersion.RELEASE_11)
public class UtilityProcessor extends AbstractProcessor {

	@Override
	public boolean process(
			Set<? extends TypeElement> annotations,
			RoundEnvironment roundingEnvironment)
	{
		Messager messager = processingEnv.getMessager();
		
		for (TypeElement te : annotations) {
			for (Element elt : roundingEnvironment.getElementsAnnotatedWith(te)) {
				if (elt.getKind().equals(ElementKind.CLASS)) {	// elt is a class
					// Check that the class is declared final
					if (! elt.getModifiers().contains(javax.lang.model.element.Modifier.FINAL)){
						messager.printMessage(Kind.WARNING,
						"The provided UtilityProcessor class is wrong.  Correct it! (final) ");
					}	
					// Check that enclosed elements are static
					for (Element elt2 : elt.getEnclosedElements()) {
						if ((!elt2.getKind().equals(ElementKind.CONSTRUCTOR)) && ! (elt2.getModifiers().contains(Modifier.STATIC))){
							messager.printMessage(Kind.WARNING,
							"The provided UtilityProcessor class is wrong.  Correct it ! (static) ");
						}
					}
				} else {
					messager.printMessage(Kind.ERROR,
							"@Utility applies to class only:", elt);
				}
			}
		}
		messager.printMessage(Kind.NOTE, "UtilityProcessor executed.");
		return true;
	}

}
