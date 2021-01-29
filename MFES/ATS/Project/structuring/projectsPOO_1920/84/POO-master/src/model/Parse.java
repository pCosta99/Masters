package model;

import exceptions.*;
import exceptions.ExcecaoUserExistente;
import exceptions.ExcecaoUserInvalido;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Parse {
    private List<String> file;

    public Parse() {
        this.file = new ArrayList<>();
    }

    public  Parse(String str, TrazAqui model) {
        List<String> linhas = lerFicheiro(str);//alterar nome do ficheiro
        try {

            this.file = linhas.stream()
                    .map(String::trim)//.map(String::trim)//ver para que serve o trim
                    .filter(s -> s.contains(":") && s.contains(","))
                    .map(linha -> this.parser(linha, model))
                    .collect(Collectors.toList());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    public String parser(String linha, TrazAqui model) {
        String[] linhaPartida = linha.split(":");
        String classe = linhaPartida[0];
        String[] contem = linhaPartida[1].split(",");


        try{
            switch (classe) {
                case "Cliente":
                    if(contem.length != 7)
                        break;

                    try{
                        Cliente u = parseCliente(contem);
                        model.addUser(u);// criar um Utilizador
                    } catch (ExcecaoUserExistente excecaoUserExistente) {
                        excecaoUserExistente.printStackTrace();
                    }
                    break;


                case "Loja":
                    if(contem.length != 7)
                        break;
                    try {
                        Loja l = parseLoja(contem);
                        model.registaLoja(l);
                    } catch (ExcecaoRegisto excecaoRegisto) {
                        excecaoRegisto.printStackTrace();
                    }
                    break;



                case "Voluntario":
                    if(contem.length != 11)
                        break;

                    try {
                        Voluntario v = parseVoluntario(contem);
                        model.addUser(v);
                        if(v.isStatus())
                            model.getVolD().add(v);
                    } catch (ExcecaoUserExistente excecaoUserExistente) {
                        excecaoUserExistente.printStackTrace();
                    }
                    break;

                case "Transportadora":
                    if(contem.length != 6)
                        break;

                    EmpresaTransporte e = parseTransportadora(contem);
                    model.registaEmpresaTransporte(e);
                    break;

                case "EncomendaRealizada":
                    if(contem.length != 8)
                        break;

                        String id = contem[0];
                        String idsegundo = id.substring(1); //get do id de empresa

                        LocalDateTime data = LocalDateTime.parse(contem[1]);
                        String idCliente = contem[2];
                        Cliente cl = (Cliente) model.getUser(idCliente);
                        boolean special = Boolean.parseBoolean(contem[3]);
                        double gpsx = Double.parseDouble(contem[4]); //
                        double gpsy = Double.parseDouble(contem[5]); // destino
                        Coordenadas c = new Coordenadas(gpsx, gpsy);
                        int peso = Integer.parseInt(contem[6]);
                        double taxa = Double.parseDouble(contem[7]);

                        String[] produtos = contem[8].split(";");
                        List<Produto> lista = new ArrayList<>();
                        for (int i = 0; i < produtos.length; i++) {
                            Produto p = model.procuraProdutoLoja2(produtos[i]);
                            lista.add(p);
                        }

                        Encomenda encomenda = new Encomenda(idsegundo, cl, data, special, c, taxa, lista);
                        ((Cliente) model.getUser(idCliente)).addEncomendaRegisto(encomenda);

                        switch (id.charAt(0)){
                            case 'e':
                                EmpresaTransporte empresa = model.getEmpresas().get(idsegundo);
                                model.addEncomendaAEmpresa(empresa,encomenda);
                                model.addEncomendaHistoricoACliente(cl, encomenda);
                                break;



                            case 'v':
                                Voluntario v = (Voluntario) model.getUtilizadores().get(idsegundo);
                                model.addEncomendaHistoricoAVoluntario(v,encomenda);
                                model.addEncomendaHistoricoACliente(cl, encomenda);
                                break;


                            default:
                                throw new ExcecaoNumeroErradoArgumentos();
                        }



                    break;


                case "EncomendaPending":
                    if(contem.length != 8)
                        break;
                    try {
                        String ide = contem[0];
                        String id2 = ide.substring(1); //get do id de empresa
                        LocalDateTime data1 = LocalDateTime.parse(contem[1]);
                        String idClient = contem[2];
                        Cliente cli = (Cliente) model.getUser(idClient);
                        boolean especial = Boolean.parseBoolean(contem[3]);
                        double gpsx1 = Double.parseDouble(contem[4]); //
                        double gpsy1 = Double.parseDouble(contem[5]); // destino
                        Coordenadas coord = new Coordenadas(gpsx1, gpsy1);
                        int peso1 = Integer.parseInt(contem[6]);
                        double t = Double.parseDouble(contem[7]);

                        String[] produtos1 = contem[8].split(";");
                        List<Produto> lista1 = new ArrayList<>();
                        for (int i = 0; i < produtos1.length; i++) {
                            Produto p = model.procuraProdutoLoja2(produtos1[i]);
                            lista1.add(p);
                        }

                        Encomenda encomenda2 = new Encomenda(ide, cli, data1, especial, coord, t, lista1);
                        ((Cliente) model.getUser(idClient)).addEncomendaPending(encomenda2);
                        //EmpresaTransporte empresa = model.getEmpresas().get(id2);
                        System.out.println(ide);
                        switch (ide.charAt(0)){
                            case 'e':
                                EmpresaTransporte empresa1 = model.getEmpresas().get(id2);
                                model.addEncomendaAEmpresa(empresa1,encomenda2);
                                model.addEncomendaPendingACliente(cli,encomenda2);
                                break;

                            case 'v':
                                Voluntario v = (Voluntario) model.getUtilizadores().get(id2);
                                model.addEncomendaPendingAVoluntario(v,encomenda2);
                                model.addEncomendaPendingACliente(cli,encomenda2);
                                break;

                            default:
                                throw new ExcecaoNumeroErradoArgumentos();
                        }

                    } catch (ExcecaoUserInvalido | NumberFormatException | ExcecaoProdutoInexistente | ExcecaoNumeroErradoArgumentos excecaoUserInvalido) {
                        excecaoUserInvalido.printStackTrace();
                    }

                    break;


                case "ProdutoMedico":
                    if(contem.length != 6)
                        break;
                    try {
                        ProdutoMedico pm = parseProdutoMedico(contem);
                        model.getLojaEspecifica(pm.getIdLoja()).addProduto(pm);
                    } catch (ExcecaoLojaInexistente excecaoLojaInexistente) {
                        excecaoLojaInexistente.printStackTrace();
                    }
                    break;

                case "ProdutoNormal":
                    if(contem.length != 6)
                        break;
                    try {
                        ProdutoNormal pn = parseProdutoNormal(contem);
                        model.getLojaEspecifica(pn.getIdLoja()).addProduto(pn);
                    } catch (ExcecaoLojaInexistente excecaoLojaInexistente) {
                        excecaoLojaInexistente.printStackTrace();
                    }
                    break;

                default:
                    System.out.println("Linha Invalida");
                    break;
            }

        } catch (ExcecaoRegisto | ExcecaoProdutoInexistente | ExcecaoUserInvalido | ExcecaoNumeroErradoArgumentos excecaoUserExistente) {
            excecaoUserExistente.printStackTrace();
        }
        //System.out.println("importado!");
        return linha;
    }


                                

  public Cliente parseCliente(String[] campos){

        //String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas c = new Coordenadas(gpsx,gpsy);
        String mail = campos[4];
        String pass = campos[5];
        String morada = campos[6];
        List<Encomenda> registo = new ArrayList<>();
        List<Encomenda> espera  = new ArrayList<>();



        return new Cliente(codUtilizador,nome,mail,pass,morada,c,registo,espera);
  }

  public Loja parseLoja(String[] campos){

        //String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas coord = new Coordenadas(gpsx,gpsy);

        boolean especial = Boolean.parseBoolean(campos[4]);
        //float faturacao=0;
        int filaDeEspera=Integer.parseInt(campos[5]);
        double tempoMedioAtendimento=Double.parseDouble(campos[6]);
        List<Encomenda> pending = new ArrayList<Encomenda>();
        List<Encomenda> prontas = new ArrayList<Encomenda>();
        List<Produto> produtos  = new ArrayList<Produto>();

        return new Loja(nomeLoja,codLoja,filaDeEspera,coord,especial,tempoMedioAtendimento,pending,prontas,produtos);
  }

  public Voluntario parseVoluntario(String[] campos){

        //String[] campos = input.split(",");
        String codV = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas coord = new Coordenadas(gpsx,gpsy);
        String email = campos[4]; 
        String password = campos[5]; 

        int raio = Integer.parseInt(campos[6]);
        boolean special = Boolean.parseBoolean(campos[7]);
        boolean status = Boolean.parseBoolean(campos[8]);
        String morada = campos[9];
        List<Viagem> v = new ArrayList<>();
        int totalKms=0; 
        double tempoMedio=0;
        double velocidadeM= Double.parseDouble(campos[10]);
        List<Encomenda> reg = new ArrayList<>();
        List<Encomenda> espera = new ArrayList<>();

        return new Voluntario(codV,nome,email,password,morada,coord,raio,special,status,v,totalKms,tempoMedio,velocidadeM,reg,espera);//(reg,espera);//sem morada e codPostal
  }

   public EmpresaTransporte parseTransportadora(String[] campos){

        //String[] campos = input.split(",");
        String nome = campos[0];
        String id = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double taxa = Double.parseDouble(campos[4]);
        Coordenadas c = new Coordenadas(gpsx,gpsy);
        int capacidadeEncomendas= Integer.parseInt(campos[5]);
        List<Encomenda> reg = new ArrayList<Encomenda>();
        double facturacao=0;
        double classificacao=0;

        return new EmpresaTransporte(nome,id,reg, taxa,capacidadeEncomendas, c, classificacao);
  }

  public ProdutoNormal parseProdutoNormal(String[] campos){

        //String[] campos = input.split(",");
        String codProduto = campos[0];
        String idLoja = campos[1];
        String nome = campos[2];
        double preco = Double.parseDouble(campos[3]);
        //String idspecial = campos[3];
        double peso = Double.parseDouble(campos[4]);
        int quantidade = Integer.parseInt(campos[5]);


        return new ProdutoNormal(preco,peso,idLoja,quantidade,codProduto,nome);

  }


  public ProdutoMedico parseProdutoMedico(String[] campos){
      //String[] campos = input.split(",");
      String codProduto = campos[0];
      String idLoja = campos[1];
      String nome = campos[2];
      double preco = Double.parseDouble(campos[3]);
      //String idspecial = campos[3];
      double peso = Double.parseDouble(campos[4]);
      int quantidade = Integer.parseInt(campos[5]);

      return new ProdutoMedico(preco,peso,idLoja,quantidade,codProduto,nome);
  }





        


  public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); } //
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
  }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Parse parse = (Parse) o;
        return Objects.equals(file, parse.file);
    }

}


