package Modelos;

import Exceptions.AlreadyEvaluatedException;
import Exceptions.ProdutoInexistenteException;
import Exceptions.UserInexistenteException;
import Users.User;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collection;

public interface TrazAquiModel extends Serializable {

    Collection<Object> interpreta(int num, Collection<Object> l) throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException;

    boolean checkLoggin(String o, String o1) throws UserInexistenteException;

    User getLogged();

    void logout();
}
