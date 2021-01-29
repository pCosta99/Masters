package app.views;

import java.util.List;
import java.util.Scanner;
import app.models.Produto;

import static java.lang.System.out;

public class MenuPaginatedProdutos {

    private List<Produto> produtos;
    private Scanner scanner;
    int pageSize = 10;
    int pageNumber = 1;
    boolean optConfirmar;

    public MenuPaginatedProdutos(List<Produto> produtos, Scanner scanner) {
        this.produtos = produtos;
        this.scanner = scanner;
        this.optConfirmar = false;
    }

    public MenuPaginatedProdutos(List<Produto> produtos, Scanner scanner, boolean optConfirmar) {
        this.produtos = produtos;
        this.scanner = scanner;
        this.optConfirmar = optConfirmar;
    }

    public String print() {
        String op;

        if (produtos.isEmpty()) {
            return null;

        }

        int lastIndex;
        if (((pageSize * pageNumber) - 1) < produtos.size()) {
            lastIndex = (pageSize * pageNumber) - 1;

        } else {
            lastIndex = produtos.size();
        }

        for (Produto produto : produtos.subList(((pageNumber - 1) * pageSize), lastIndex)) {
            out.println("* [" + produto.getCodProduto() + "] - " + produto.getDescricao() + " - "
                    + produto.getPrecoUni() + "€/unid  *");
        }

        if (optConfirmar) {
            out.println("*  [CONFIRMAR] Confirmar Encomenda         *");
        }
        out.println("*  [0] Sair                                *");
        out.println("******************************************\n");
        if (pageNumber != 1) {
            out.println("Página anterior(-)\n");

        }
        if (produtos.size() > (pageNumber * pageSize)) {
            out.println("Próxima página(+)\n");

        }
        out.println("Opção: ");
        op = scanner.next();

        if (op.equals("-")) {

            if (pageNumber != 1) {
                pageNumber--;
            }
        } else if (op.equals("+") && (produtos.size() > (pageNumber * pageSize))) {
            pageNumber++;
        }

        return op;
    }

}
