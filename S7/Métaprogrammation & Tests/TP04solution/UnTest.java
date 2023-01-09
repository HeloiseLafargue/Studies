import java.lang.annotation.*;

/**
  * ATest : indiquer 
  *
  * @author	Xavier CrÃ©gut <Prenom.Nom@enseeiht.fr>
  */

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface UnTest {
	boolean enabled() default true;
	Class<? extends Throwable> expected() default NoException.class;
				// On ne peut pas mettre null, on crÃ©e donc une exception ici
				// qui ne servira que de valeur par dÃ©faut.

	static final class NoException extends Throwable {
		private NoException() {}	// Elle ne sera jamais instanciÃ©e.
	}



}
