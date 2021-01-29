package Encomenda;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

public class Encomenda implements Serializable
{
        private String enc;
        private String user;
        private String loja;
        private double peso;
        private Map <String, LinhaEncomenda> produtos;
        
        private int aceitaCusto;// 0-nao ,1-sim
        private int medicamentos;//se e de medicamentos
       
        private int estado;//0->encomenda nao pronta//->1 encomenda pronta//-1 encomenda entregue
    
        private String nomTrans;//Se e empresa ou voluntario
        private double tempo;
        private double custo;//= distancia CustoKM(Transporte.Empresa->loja->User)*(peso*0.1)
        private LocalDateTime data;
        private double classificacao;// atribuida pelo user de 0 a 10    
    
    public Encomenda(){
        this.enc= "";
        this.loja= "";
        this.user= "";
        this.peso=0;
        this.produtos= new HashMap<>();
        this.aceitaCusto=0;
        this.medicamentos=0;
        this.estado=-1;//-1->Nao esta pronta a ser entregue,0->pronta a ser enregue, 1->Entregue
        this.nomTrans= "";
        this.tempo=-1;
        this.custo=0;
        this.data=LocalDateTime.now();
        this.classificacao=0;
    }
    public Encomenda(int ac,LocalDateTime dt,double fat,int es,String tr){
        this.enc= "";
        this.loja= "";
        this.user= "";
        this.peso=0;
        this.produtos= new HashMap<>();
        this.aceitaCusto=ac;
        this.medicamentos=0;
        this.estado=es;//-1->Nao esta pronta a ser entregue,0->pronta a ser enregue, 1->Entregue
        this.nomTrans=tr;
        this.tempo=-1;
        this.custo=fat;
        this.data=dt;
        this.classificacao=0;
    }

    public String getEnc(){return this.enc;}
    public String getLoja(){return this.loja;}
    public String getUser(){return this.user;}
    public double getPeso(){return this.peso;}

    public int getAceitaCusto(){return this.aceitaCusto;}

    public int getEstado(){return this.estado;}
    public String getNomTrans(){return this.nomTrans;}

    public double getCusto(){return this.custo;}

    public LocalDateTime getData(){return this.data;}
    
    public void setEnc(String enc){this.enc=enc;}
    public void setLoja(String loja){
        this.loja=loja;
    }
    public void setUser(String user){this.user=user;}
    public void setPeso(double peso){
        this.peso=peso;
    }

    public void setAceitaCusto(int aceitaCusto){this.aceitaCusto=aceitaCusto;}
    public void setMedicamentos(int m){this.medicamentos=m;}
    public void setEstado(int estado){this.estado=estado;}
    public void setNomTrans(String trans){this.nomTrans=trans;}
    public void setTempo(double tempo){this.tempo=tempo;}
    public void setCusto(double custo){this.custo=custo;}
    public void setData(LocalDateTime data){this.data=data;}
    public void setClassificacao(double classificacao){this.classificacao=classificacao;}

    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append("\nEncomenda.Encomenda: ").append(this.enc);
        sb.append("\nUtilizador.Loja: ").append(this.loja);
        sb.append("\nUtilizador.Utilizador: ").append(this.user);
        sb.append("\nPeso: ").append(this.peso);
        sb.append("\nProdutos:\n");
        for(LinhaEncomenda c: this.produtos.values()){
            sb.append(c.toString()).append("\n");
        }
        sb.append("\nAceita o Custo: ").append(this.aceitaCusto);
        sb.append("\nContem Medicamentos: ").append(this.medicamentos);
        sb.append("\nEstado da Encomenda.Encomenda: ").append(this.estado);
        sb.append("\nTransporte.Transporte: ").append(this.nomTrans);
        sb.append("\nCusto: ").append(this.custo).append("euros");
        sb.append("\nTempo: ").append(this.tempo).append("minutos");
        sb.append("\nTData: ").append(this.data);
        sb.append("\nClassificaçao: ").append(this.classificacao).append("\n\n\n");
        return sb.toString();
    }


    public void addLinhaEncomenda(LinhaEncomenda linha){
        this.produtos.put(linha.getReferencia(),linha.clone());
    }
    
    public float pesoEncomenda(){
        float j=0;
        for(LinhaEncomenda c: this.produtos.values()){
            j+=(c.getQuantidade()*c.getPeso());
        }
        return j;
        }


}