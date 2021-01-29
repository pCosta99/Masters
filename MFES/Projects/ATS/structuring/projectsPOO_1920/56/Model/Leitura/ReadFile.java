package Model.Leitura;

import Model.*;
import Model.Catalogos.CatalogoProds;
import Model.Catalogos.ICatalogoProds;
import Model.Encomendas.Encomenda;
import Model.Encomendas.IEncomenda;
import Model.Logins.ILogin;
import Model.Logins.Login;
import Model.Tipos.*;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

public class ReadFile implements IReadFile {
    IUser user;
    IVoluntario voluntario;
    IEmpresa empresa;
    ILoja loja ;
    IEncomenda enco;
    ILogin login;
    ICatalogoProds catalogoProds;


    public ReadFile(){
        this.user = new User();
        this.voluntario = new Voluntario();
        this.empresa = new Empresa();
        this.loja = new Loja();
        this.enco = new Encomenda();
        this.login = new Login();
        this.catalogoProds = new CatalogoProds();

    }

    public void leitura(String f, ISistema sistema){
        List<String> linhas = new ArrayList<>();
        if (f!=null) {                  //Verificar que o input não é nulo
            linhas = read(f);           //ler ficheiro e colocar em list
        }
        String[] linhaPartida;
        int i;
        String aux;
        String id="";
        int conta1=0,conta2=0,conta3=0,conta4=0,conta5=0,conta6=0;

        for (i = 0; i < linhas.size(); i++) {
            if (linhas.get(i)!=null) {
                linhaPartida = linhas.get(i).split(":", 2);
                if (linhaPartida[0]!=null) {

                    ILogin login = new Login();
                    switch(linhaPartida[0]) {

                        case ("Utilizador"): {
                            ITipo user = new User();
                            aux = linhaPartida[1];
                            parseTipo(aux,user);
                            sistema.getUsers().addTipo(user);
                            id = user.getId();


                            conta2++;
                            break;
                        }

                        case ("Voluntario"): {
                            ITipo voluntario = new Voluntario();
                            aux = linhaPartida[1];
                            parseTipo(aux,voluntario);
                            sistema.getVoluntarios().addTipo(voluntario);
                            id = voluntario.getId();

                            conta3++;
                            break;
                        }

                        case ("Transportadora"): {
                            ITipo empresa = new Empresa();
                            aux = linhaPartida[1];
                            parseTipo(aux,empresa);
                            sistema.getEmpresas().addTipo(empresa);
                            id = empresa.getId();

                            conta4++;
                            break;
                        }

                        case ("Loja"): {
                            ITipo loja = new Loja();
                            aux = linhaPartida[1];
                            parseTipo(aux,loja);
                            sistema.getLojas().addTipo(loja);
                            id = loja.getId();

                            conta5++;
                            break;
                        }

                        case ("Encomenda"): {
                            IEncomenda enco = new Encomenda();
                            aux = linhaPartida[1];
                            enco.criaEncomenda(aux,catalogoProds);
                            enco.setHoraInicial(LocalTime.now());
                            sistema.getFilaEspera().addEncomenda(enco);
                            sistema.setCatalogo(catalogoProds);
                            conta6++;
                            break;
                        }

                        case ("Aceite"):{
                            aux = linhaPartida[1];
                            sistema.addAceite(aux);
                            conta1++;
                            break;
                        }

                        default: {
                            System.out.println("Não foi registado -> " + linhaPartida[0]);
                            break;
                        }

                    }
                    login.setLogin(id);
                    sistema.getLogins().addLogin(login,id);
                }
            }
        }
        //Outras funções para definir o inicio do programa
        sistema.StockLoja();

        //        ------------------TESTES----------------------
        System.out.println(sistema.toString());
        System.out.printf("\nNúmero de Encomendas aceites: %d\n",conta1);
        System.out.printf("Número de Utilizadores: %d\n",conta2);
        System.out.printf("Número de Voluntários: %d\n",conta3);
        System.out.printf("Número de Empresas Transportadoras: %d\n",conta4);
        System.out.printf("Número de Lojas: %d\n",conta5);
        System.out.printf("Número de Encomendas: %d\n",conta6);
        System.out.printf("Número de Produtos: %d\n",catalogoProds.totalProds());
        //System.out.println(sistema.getListaLojas().toString());

    }

    public static void parseTipo(String linha, ITipo tipo){
        if(tipo instanceof Loja){
            String[] id = linha.split(",");
            tipo.setId(id[0]);
            tipo.setNome(id[1]);
        }
        else {
            String[] id = linha.split(",");
            tipo.setId(id[0]);

            tipo.setX(Float.parseFloat(id[2]));
            tipo.setY(Float.parseFloat(id[3]));

            String[] textoSeparado = id[1].split(" ");
            String nameAux = textoSeparado[0] + " " + textoSeparado[textoSeparado.length - 1];
            tipo.setNome(nameAux);

            if (tipo instanceof Voluntario) {
                float r = Float.parseFloat(id[4]);
                Voluntario x = (Voluntario) tipo;
                x.setRadius_volunteer(r);
            }
            if (tipo instanceof Empresa) {
                Empresa e = (Empresa) tipo;
                e.setNif(Integer.parseInt(id[4]));
                e.setRaio(Double.parseDouble(id[5]));
                e.setPreco(Double.parseDouble(id[6]));
            }
        }
    }
    public static List<String> read (String f){
        int i;
        List<String> res = new ArrayList<>();
        i = 0;
        try {
            FileReader buf = new FileReader(f);
            BufferedReader lerBuf = new BufferedReader(buf);
            String linha = lerBuf.readLine();
            while (linha != null) {
                linha = lerBuf.readLine();
                res.add(linha);
                i++;
            }
        }
        catch (IOException e){
            e.getMessage();
        }
        return res;
    }
}
