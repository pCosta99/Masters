package View;

import java.util.function.Predicate;

/**Classe que gaurda um predicado assim como uma string
 * que o define (instrução)
 */

public class BetterPredicate {
    private Predicate<Object> predicate;
    private String instruction;

    public BetterPredicate(Predicate<Object> predicate, String instruction) {
        this.predicate = predicate;
        this.instruction = instruction;
    }

    public boolean test(Object o) {
        return predicate.test(o);
    }

    public String getInstruction() {
        return instruction;
    }
}
