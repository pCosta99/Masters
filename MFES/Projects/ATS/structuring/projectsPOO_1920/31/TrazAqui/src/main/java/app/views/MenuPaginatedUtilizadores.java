package app.views;

import java.util.List;
import java.util.Scanner;
import app.interfaces.IInfo;
import static java.lang.System.out;

public class MenuPaginatedUtilizadores {

    private List<IInfo> users;
    private Scanner scanner;
    int pageSize = 10;
    int pageNumber = 1;

    public MenuPaginatedUtilizadores(List<IInfo> users, Scanner scanner) {
        this.users = users;
        this.scanner = scanner;
    }

    public String print() {
        String op;
        if (users.isEmpty())
            return null;


        int lastIndex;
        if (((pageSize * pageNumber) - 1) < users.size()) {
            lastIndex = (pageSize * pageNumber) - 1;

        } else {
            lastIndex = users.size();
        }

        for (IInfo user : users.subList(((pageNumber - 1) * pageSize), lastIndex)) {
            out.println("* [" + user.getEmail() + "] - " + user.getNome() + " *");
        }

        out.println("*  [0] Sair                                *");
        out.println("******************************************\n");
        if (pageNumber != 1) {
            out.println("Página anterior(-)\n");

        }
        if (users.size() > (pageNumber * pageSize)) {
            out.println("Próxima página(+)\n");

        }
        out.println("Opção: ");
        op = scanner.next();

        if (op.equals("-")) {

            if (pageNumber != 1) {
                pageNumber--;
            }
        } else if (op.equals("+") && (users.size() > (pageNumber * pageSize))) {
            pageNumber++;
        }

        return op;
    }

}
