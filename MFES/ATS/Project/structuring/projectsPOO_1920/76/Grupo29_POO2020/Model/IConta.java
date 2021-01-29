package Model;

import java.util.List;

public interface IConta {
    /**
     * Indica o email associado à conta.
     * @return o email.
     */
    String getEmail();

    /**
     * Indica a palavra-passe da conta.
     * @return a palavra-passe.
     */
    String getPassword();

    /**
     * Indica o id da entidade associada à conta.
     * @return o id.
     */
    String getId();

    /**
     * Indica o tipo de conta.
     * @return o caractére que indica o tipo de conta.
     */
    char getTipo();

    /**
     * Adiciona uma notificação a uma conta.
     * @param message notificação.
     */
    void addNotificacao(String message);

    /**
     * Indica todas as notificações de uma conta.
     * @return as notificações.
     */
    List<String> getNotificacoes();
}