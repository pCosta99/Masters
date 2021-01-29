import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

public class ControllerUtilizador implements Serializable {
    private Utilizador utilizador;
    private Sistema sistema;
    private ViewUtilizador view;



    public ControllerUtilizador(Utilizador utilizador, Sistema sistema) {
        this.utilizador = utilizador;
        this.sistema = sistema;
        this.view = new ViewUtilizador();
    }


    public Utilizador getUtilizador() {
        return utilizador;
    }

    public void setUtilizador(Utilizador utilizador) {
        this.utilizador = utilizador;
    }

    public Sistema getSistema() {
        return sistema;
    }

    public void setSistema(Sistema sistema) {
        this.sistema = sistema;
    }

    public ViewUtilizador getView() {
        return view;
    }

    public void setView(ViewUtilizador view) {
        this.view = view;
    }


    public Encomenda criaEncomenda(Encomenda encomenda){
        Set<LinhaEncomenda> produtos_encomenda = new HashSet<>();
        List<LinhaEncomenda> produtos_loja = Loja.getProdutos();
        this.view.FazerEncomenda_mostrarProduto(produtos_loja);

        double peso=0.0, pesoTotal=0.0;
        String codProduto = "";

        while(!codProduto.equals("q")){
            codProduto = Input.lerString();
            if(!codProduto.equals("q")) {
                if (!Loja.temProduto(codProduto)) {
                    this.view.PrintMensagem("O código de produto inserido não existe. Tente novamente.");
                    codProduto = Input.lerString();
                } else {
                    peso = Input.lerDouble();
                    while (peso < 0) {
                        this.view.PrintMensagem("Peso Invalido. Por favor insira um peso positivo");
                        peso = Input.lerDouble();
                    }
                    pesoTotal+=peso;
                    LinhaEncomenda novaLinhaEncomenda = Loja.getLinhaEncomenda(codProduto);
                    novaLinhaEncomenda.setQuantidade(peso);
                    encomenda.addLinhaEncomenda(novaLinhaEncomenda);
                    //codProduto=Input.lerString();
                }
            }
        }
        encomenda.setPeso((float) pesoTotal);
        return encomenda;
    }

    public void classificar(String codigoEncomenda){
        this.view.classificar(this.sistema.quemEntregaEncomenda(codigoEncomenda));
        String classificacao = "";
        int classificar, flag=0;

    while(flag==0){
        classificacao = Input.lerString();
        if(classificacao.equals("q")) return;
        flag=1;
            try {
                classificar = Integer.parseInt(classificacao);
                while (classificar<0 ||classificar>10){
                    this.view.PrintMensagem("Valor Inválido. Insira um valor entre 0 e 10.");
                    classificacao = Input.lerString();
                    if(classificacao.equals("q")) return;
                    classificar = Integer.parseInt(classificacao);
                }
                this.sistema.classificar(codigoEncomenda, classificar);
            } catch (NumberFormatException e) {
                flag=0;
                this.view.PrintMensagem("Classificacao inválida. Tente novamente.");
            }
        }
    }




    public int getAno(){
        int ano=Input.lerInt();
        int atual= LocalDateTime.now().getYear();
        while(ano>atual|| ano<0){
            ano=Input.lerInt();
        }
        return ano;
    }
    public int getMes(){
        int mes=Input.lerInt();

        while(mes>12|| mes<1){
            mes=Input.lerInt();
        }
        return mes;
    }

    public int getDia(int mes){
        int dia=Input.lerInt();

        if(mes==1||mes==3||mes==5||mes==7||mes==8||mes==10||mes==12){
            while(dia<1|| dia>31){
                this.view.PrintMensagem("Dia inválido.  Tente Novamente, introduzindo um valor entre 1 e 31.");
                dia=Input.lerInt();
            }
        } else{
            if(mes==4||mes==6||mes==9||mes==11){
                while(dia<1|| dia>30){
                    this.view.PrintMensagem("Dia inválido.  Tente Novamente, introduzindo um valor entre 1 e 30.");
                    dia=Input.lerInt();
                }
            }
        }
        if (mes==2)
            while(dia<1||dia>29){
                this.view.PrintMensagem("Dia inválido. Tente Novamente, introduzindo um valor entre 1 e 29.");
                dia=Input.lerInt();
            }
        return dia;
    }

    public int getHora(){
        int hora=Input.lerInt();
        while(hora<0||hora>24){
            this.view.PrintMensagem("Hora inválida. Tente Novamente, introduzindo um valor entre 1 e 24.");
            hora=Input.lerInt();
        }
        return hora;
    }

    public LocalDateTime getData(){
        int ano, mes, dia, hora;
        ano=getAno();
        mes=getMes();
        dia=getDia(mes);
        hora=getHora();
        return LocalDateTime.of(ano,mes,dia,hora,0);

    }

    public void extratoVoluntario(){

        this.view.EscolheVoluntarioParaExtrato(this.sistema.getVoluntarios());
        String codigo=Input.lerString();
        while(!this.sistema.eVoluntario(codigo)){
            this.view.PrintMensagem("Código Inválido. Tente Novamente");
            codigo=Input.lerString();
        }

        this.view.IntervaloDeTempoParaExtratos();
        LocalDateTime inicio = getData();
        LocalDateTime fim = getData();
        while(inicio.isAfter(fim)){
            this.view.PrintMensagem("Data inicial ocorre após data final. Tente novamente.");
            inicio = getData();
            fim = getData();
        }
        List<Encomenda> extrato= this.sistema.extratoVoluntario(codigo,inicio,fim);
        int total=extrato.size();
        this.view.extrato(total,extrato);
    }

    public void extratoTransportadora(){

        this.view.EscolheTransportadoraParaExtrato(this.sistema.getTransportadoras());
        String codigo=Input.lerString();
        while(!this.sistema.eTransportadora(codigo)){
            this.view.PrintMensagem("Código Inválido. Tente Novamente");
            codigo=Input.lerString();
        }

        this.view.IntervaloDeTempoParaExtratos();
        LocalDateTime inicio = getData();
        LocalDateTime fim = getData();
        while(inicio.isAfter(fim)){
            this.view.PrintMensagem("Data inicial ocorre após data final. Tente novamente.");
            inicio = getData();
            fim = getData();
        }
        List<Encomenda> extrato= this.sistema.extratoTransportadora(codigo,inicio,fim);
        int total=extrato.size();
        this.view.extrato(total,extrato);
    }




    public void extratos(){
        this.view.menuDeExtratos();
        int opcao= 3;

        while(opcao!=0 && opcao!=1 && opcao!=2) {

            opcao= Input.lerInt();
            switch (opcao) {
                case 0:
                    break;

                case 1:
                    extratoVoluntario();
                    break;
                case 2:
                    extratoTransportadora();


                    break;

                default:
                    this.view.PrintMensagem("Opção inválida. Tente novamente.");
                    opcao = Input.lerInt();
            }
        }
    }


    public void inicio() throws InterruptedException {
        int opcao = 1;

        while(opcao!=0) {
            this.view.menu();
            opcao = Input.lerInt();
            switch (opcao) {
                case 1:
                    Encomenda encomenda = new Encomenda();
                    encomenda.setCodEncomenda(Encomenda.novoCodigo());
                    List<Loja> lojas = this.sistema.getLojas();

                    this.view.fazerEncomenda_mostrarLojas(lojas);
                    String loja = Input.lerString();
                    while (!this.sistema.temLoja(loja)) {
                        this.view.PrintMensagem("Código de loja inválido. Tente novamente.");
                        loja = Input.lerString();
                    }
                    encomenda.setCodLoja(loja);
                    encomenda.setCodUtilizador(this.utilizador.getCodigo());
                    encomenda = criaEncomenda(encomenda);

                    this.sistema.addEncomenda(encomenda);

                    this.sistema.distribuiEncomendas();    
                    this.view.tempoEspera(this.utilizador.getTempoUltimaEncomenda());
                    classificar(encomenda.getCodEnc());
                    break;

                case 2:
                    extratos();
                    TimeUnit.SECONDS.sleep(4);
                    break;
                case 0:
                    break;

                default:
                    this.view.PrintMensagem("Opção inválida. Tente novamente.");
            }
        }
    }


}
