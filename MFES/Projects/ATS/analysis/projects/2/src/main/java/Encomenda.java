import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.Iterator;
import java.time.LocalDateTime;  
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
        private double custo;//= distancia CustoKM(Empresa->loja->User)*(peso*0.1)
        private LocalDateTime data;
        private double classificacao;// atribuida pelo user de 0 a 10    
    
    public Encomenda(){
        this.enc=new String();
        this.loja=new String();
        this.user=new String();
        this.peso=0;
        this.produtos=new HashMap<String, LinhaEncomenda>();
        this.aceitaCusto=0;
        this.medicamentos=0;
        this.estado=-1;//-1->Nao esta pronta a ser entregue,0->pronta a ser enregue, 1->Entregue
        this.nomTrans=new String();
        this.tempo=-1;
        this.custo=0;
        this.data=LocalDateTime.now();
        this.classificacao=0;
    }
    public Encomenda(int ac,LocalDateTime dt,double fat,int es,String tr){
        this.enc=new String();
        this.loja=new String();
        this.user=new String();
        this.peso=0;
        this.produtos=new HashMap<String, LinhaEncomenda>();
        this.aceitaCusto=ac;
        this.medicamentos=0;
        this.estado=es;//-1->Nao esta pronta a ser entregue,0->pronta a ser enregue, 1->Entregue
        this.nomTrans=tr;
        this.tempo=-1;
        this.custo=fat;
        this.data=dt;
        this.classificacao=0;
    }
    public Encomenda(String enc,String user,String loja,double peso,
    Map <String,LinhaEncomenda> produtos,int aceitaCusto,int medicamentos,
    int estado,String nomTrans,double tempo,double custo,LocalDateTime data,double classificacao){
        this.enc=enc;
        this.user=user;
        this.loja=loja;
        this.peso=peso;
        this.produtos=new HashMap<String, LinhaEncomenda>();
        this.setProdutos(produtos); 
        this.aceitaCusto=aceitaCusto;
        this.medicamentos=medicamentos;
        this.estado=estado;
        this.nomTrans=nomTrans;
        this.tempo=tempo;
        this.custo=custo;
        this.data=data;
        this.classificacao=classificacao;
    }
    public Encomenda (Encomenda enc){
        this.enc=enc.getEnc();
        this.loja=enc.getLoja();
        this.user=enc.getUser();
        this.peso=enc.getPeso();
        this.produtos=enc.getProdutos();
        this.aceitaCusto=enc.getAceitaCusto();
        this.medicamentos=enc.getMedicamentos();
        this.estado=enc.getEstado();//-1->Nao esta pronta a ser entregue,0->pronta a ser enregue, 1->Entregue
        this.nomTrans=enc.getNomTrans();
        this.tempo=enc.getTempo();
        this.custo=enc.getCusto();
        this.classificacao=enc.getClassificacao();
    }
    
    public String getEnc(){return this.enc;}
    public String getLoja(){return this.loja;}
    public String getUser(){return this.user;}
    public double getPeso(){return this.peso;}
    public Map<String,LinhaEncomenda> getProdutos(){    
       Map<String,LinhaEncomenda> res=this.produtos.values().stream()
               .collect(Collectors.toMap((produtos)->produtos.getReferencia(),
                                          (produtos)->produtos.clone()));
            return res;
    }
    public int getAceitaCusto(){return this.aceitaCusto;}
    public int getMedicamentos(){return this.medicamentos;}
    public int getEstado(){return this.estado;}
    public String getNomTrans(){return this.nomTrans;}
    public double getTempo(){return this.tempo;}
    public double getCusto(){return this.custo;}
    public double getClassificacao(){return this.classificacao;}
    public LocalDateTime getData(){return this.data;}
    
    public void setEnc(String enc){this.enc=enc;}
    public void setLoja(String loja){
        this.loja=loja;
    }
    public void setUser(String user){this.user=user;}
    public void setPeso(double peso){
        this.peso=peso;
    }
    public void setProdutos(Map<String,LinhaEncomenda> prod){
       this.produtos=new HashMap<>();
            prod.values().forEach(l -> this.produtos.put(l.getReferencia(),l.clone()));
    }
    public void setAceitaCusto(int aceitaCusto){this.aceitaCusto=aceitaCusto;}
    public void setMedicamentos(int m){this.medicamentos=m;}
    public void setEstado(int estado){this.estado=estado;}
    public void setNomTrans(String trans){this.nomTrans=trans;}
    public void setTempo(double tempo){this.tempo=tempo;}
    public void setCusto(double custo){this.custo=custo;}
    public void setData(LocalDateTime data){this.data=data;}
    public void setClassificacao(double classificacao){this.classificacao=classificacao;}
    
    public void preparaEnc(){this.estado=1;}
    
    public String toString(){
        StringBuilder sb= new StringBuilder();
        sb.append("\nEncomenda: "+this.enc);
        sb.append("\nLoja: " +this.loja);
        sb.append("\nUtilizador: " +this.user);
        sb.append("\nPeso: " +this.peso);
        sb.append("\nProdutos:\n");
        for(LinhaEncomenda c: this.produtos.values()){
            sb.append(c.toString()+"\n");
        }
        sb.append("\nAceita o Custo: "+this.aceitaCusto);
        sb.append("\nContem Medicamentos: "+this.medicamentos);
        sb.append("\nEstado da Encomenda: "+this.estado);
        sb.append("\nTransporte: "+this.nomTrans);
        sb.append("\nCusto: "+this.custo+"euros");
        sb.append("\nTempo: "+this.tempo+"minutos");
        sb.append("\nTData: "+this.data);
        sb.append("\nClassificaçao: "+this.classificacao+"\n\n\n");
        return sb.toString();
    }
    
    //equals
    
    public Encomenda Clone(){
        return new Encomenda(this);
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
    
    public float precoEncomenda(){
        float j=0;
        for(LinhaEncomenda c:this.produtos.values()){
            j+=(c.precoLinha());
        }
        return j;
        }
    

}