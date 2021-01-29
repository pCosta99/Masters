package Projeto;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Parser {
    private List<String> file;
    private String[] type;
    private String[] args;
    private String encs;

    public Parser() {
        this.file = new ArrayList<>();
    }

    public Parser(String path, TrazAqui model) {
        try {
            this.file = Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8);
            for (String line : this.file) {
                type = line.split(":");
                args = type[1].split(",");
                //System.out.println(args[2]);
                //System.out.println(Arrays.toString(type));
                //System.out.println("-----------------");
                //System.out.println(Arrays.toString(args));

                switch (type[0]) {
                    case "Utilizador":
                        Utilizador utilizador = buildUtilizador(args);
                        if (utilizador != null) {
                            model.adicionaUtilizador(utilizador);
                            System.out.println(utilizador.toString());
                        } else System.out.println("User inválido");
                        break;
                    case "Voluntario":
                        Voluntario voluntario = buildVoluntario(args);
                        if (voluntario != null) {
                            model.adicionaUtilizador(voluntario);
                            System.out.println(voluntario.toString());
                            //System.out.println(voluntário.toString());
                        }
                        else System.out.println("Voluntario inválido");
                        break;
                    case "Loja":
                        Loja loja = buildLoja(args);
                        if (loja != null) {
                            model.adicionaUtilizador(loja);
                            model.addLojas(loja);
                            System.out.println(loja.toString());
                        }
                        else System.out.println("Loja inválida");
                        break;
                    case "Transportadora":
                        Empresa empresa = buildEmpresa(args);
                        if (empresa != null) {
                            model.addEmpresa(empresa);
                            System.out.println(empresa.toString());
                        }
                        else  System.out.println("Transportadora inválida");
                        break;
                    case "Encomenda":
                        int i = 0;
                        int length = 0;
                        ArrayList<String> arrayList = new ArrayList<String>();

                        while (args[i] != null) {
                            length = args.length;
                            for (int j = 4; j < length ; j++) {
                                arrayList.add(args[j]);
                            }
                            Encomenda encomenda = buildEncomenda(args[0],args[1],args[2],args[3], arrayList);
                            model.addEncomenda(encomenda);
                            System.out.println(encomenda);
                            break;
                        }
                    case "Aceite":
                        Encomenda encAceite = buildEncomendaAceite(args);
                        if (encAceite != null) {
                            model.adicionaEncAceite(encAceite);
                        }
                        else System.out.println("Encomenda Aceite não válida");
                        break;
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public Utilizador buildUtilizador(String[] args) {
        String user;
        String nome;
        Posicao pos;

        try {
            user = args[0];
            nome = args[1];
            double x = Double.parseDouble(args[2]);
            double y = Double.parseDouble(args[3]);
            pos = new Posicao(x, y);
        } catch (InputMismatchException | NumberFormatException e) {
            return null;
        }

        return new Utilizador(user, nome, "admin", pos);
    }

    public Voluntario buildVoluntario(String[] args) {
        String user, nome;
        Posicao pos;
        double raio;

        try {
            user = args[0];
            nome = args[1];
            pos = new Posicao(Double.parseDouble(args[2]), Double.parseDouble(args[3]));
            raio = Double.parseDouble(args[4]);
        } catch (InputMismatchException | NumberFormatException e) {
            return null;
        }
        return new Voluntario(user, nome, null, pos, raio);
    }
    public Loja buildLoja(String[] args) {
        String user;
        String nome;
        Posicao pos;

        try {
            user = args[0];
            nome = args[1];
            double x = Double.parseDouble(args[2]);
            double y = Double.parseDouble(args[3]);
            pos = new Posicao(x, y);
        } catch (InputMismatchException | NumberFormatException e) {
            return null;
        }

        return new Loja(user, nome, null, pos,null);
    }

    public Empresa buildEmpresa(String[] args){
        String user, nome;
        Posicao pos;
        int nif;
        double raio, taxa;

        try{
            user=args[0];
            nome=args[1];
            pos= new Posicao(Double.parseDouble(args[2]),Double.parseDouble(args[3]));
            nif=Integer.parseInt(args[4]);
            raio=Double.parseDouble(args[5]);
            taxa=Double.parseDouble(args[6]);
        }
        catch (InputMismatchException | NumberFormatException e){
            return  null;
        }
        return new Empresa(user,nome,null,pos,nif,raio,taxa);
    }

    public Encomenda buildEncomendaAceite(String[] args){
        String code;

        try {
            code = args[0];
        }
        catch (InputMismatchException | NumberFormatException e){
            return null;
        }
        return new Encomenda(code,null,null,null,null);
    }

    public Encomenda buildEncomenda(String cod, String user, String loja, String peso, ArrayList<String> linhaEncomenda){

        ArrayList<LinhaEncomendas> linhaEncomendas = new ArrayList<LinhaEncomendas>();
        LinhaEncomendas a = null;

        for (int i = 0; i < linhaEncomenda.size(); i+=4) {
            a = new LinhaEncomendas(linhaEncomenda.get(i),
                    linhaEncomenda.get(i+1),
                    Double.parseDouble(linhaEncomenda.get(i+2)),
                    Double.parseDouble(linhaEncomenda.get(i+3)));
            linhaEncomendas.add(a);
        }
        return new Encomenda(cod,user,loja,peso, linhaEncomendas);
    }

}
