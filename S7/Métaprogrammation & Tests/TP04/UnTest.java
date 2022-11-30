import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)

public @interface UnTest {
    public boolean enabled() default true;

    public Class<? extends Throwable> expected() default NoException.class;

    static class NoException extends Throwable {
        private NoException() {}
    }
}

