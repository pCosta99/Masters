package app.views;

import java.util.List;
import java.util.Scanner;

import app.models.RegistoEncomenda;

import static java.lang.System.out;

public class MenuPaginatedEncomendas {

    private List<RegistoEncomenda> encomendas;
    private Scanner scanner;
    private boolean listaEstado = false;
    int pageSize = 10;
    int pageNumber = 1;

    public MenuPaginatedEncomendas(List<RegistoEncomenda> encomendas, Scanner scanner) {
        this.encomendas = encomendas;
        this.scanner = scanner;
    }

    public MenuPaginatedEncomendas(List<RegistoEncomenda> encomendas, Scanner scanner,
            boolean listaEstado) {
        this.encomendas = encomendas;
        this.scanner = scanner;
        this.listaEstado = listaEstado;
    }

    public String print() {
        String op;
        if (encomendas.isEmpty()) {
            return null;

        }
        int lastIndex;
        if (((pageSize * pageNumber) - 1) < encomendas.size()) {
            lastIndex = (pageSize * pageNumber) - 1;

        } else {
            lastIndex = encomendas.size();
        }

        for (RegistoEncomenda registoEncomenda : encomendas.subList(((pageNumber - 1) * pageSize),
                lastIndex)) {
            if (listaEstado) {
                out.println("* [" + registoEncomenda.getEncomenda().getCodEnc() + "] "
                        + registoEncomenda.getEncomenda().getDataCriacao().toString() + "  "
                        + registoEncomenda.getEstadoEncomenda().name() + " *");
            } else {
                out.println("* [" + registoEncomenda.getEncomenda().getCodEnc() + "] "
                        + registoEncomenda.getEncomenda().getDataCriacao().toString() + " *");
            }
        }
        out.println("*  [0] Sair                                *");
        out.println("******************************************\n");
        if (pageNumber != 1) {
            out.println("Página anterior(-)\n");

        }
        if (encomendas.size() > (pageNumber * pageSize)) {
            out.println("Próxima página(+)\n");

        }
        out.println("Opção: ");
        op = scanner.next();

        if (op.equals("-")) {

            if (pageNumber != 1) {
                pageNumber--;
            }
        } else if (op.equals("+") && (encomendas.size() > (pageNumber * pageSize))) {
            pageNumber++;
        }

        return op;
    }

}
