import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class ControllerTransportadora implements Serializable {

    private Transportadora transportadora;
    private ViewVoluntarioTransportadora view;

    public ControllerTransportadora(Transportadora transportadora) {
        this.transportadora = transportadora;
        this.view = new ViewVoluntarioTransportadora();
    }

    public Transportadora getTransportadora() {
        return transportadora;
    }

    public void setTransportadora(Transportadora transportadora) {
        this.transportadora = transportadora;
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

    public void extrato(){

        this.view.IntervaloDeTempoParaExtratos();
        LocalDateTime inicio = getData();
        LocalDateTime fim = getData();
        while(inicio.isAfter(fim)){
            this.view.PrintMensagem("Data inicial ocorre após data final. Tente novamente.");
            inicio = getData();
            fim = getData();
        }
        List<Encomenda> extrato = this.transportadora.ListaExtratoViagens(inicio,fim);
        int total=extrato.size();
        double faturado = this.transportadora.totalFaturadoPeriodo(inicio,fim);
        this.view.extratoTransportadora(total,extrato, faturado);
    }

    public void faturado(){
        double faturado=this.transportadora.totalFaturado();
        this.view.totalFarurado(faturado);
    }


    public void inicio() throws InterruptedException {


        int opcao=1;
        while(opcao!=0) {
            this.view.menuTransportadora();
            opcao=Input.lerInt();
            switch (opcao) {

                case 0:
                    break;

                case 1:
                    extrato();
                    TimeUnit.SECONDS.sleep(3);
                    break;

                case 2:
                    int classificacao = this.transportadora.getClassificacaoMedia();
                    int total = this.transportadora.getNrClassificacoes();
                    this.view.classificacao(classificacao, total);
                    TimeUnit.SECONDS.sleep(3);
                    break;

                case 3:
                    this.view.totalDeEntregas(this.transportadora.getTotalEncomendasRealizadas());
                    TimeUnit.SECONDS.sleep(3);
                    break;

                case 4:
                    faturado();
                    TimeUnit.SECONDS.sleep(3);
                    break;

                default:
                    this.view.PrintMensagem("Opção inválida. Tente novamente.");
            }
        }
    }


}
