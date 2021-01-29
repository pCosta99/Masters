
/**
 * Escreva a descrição da classe GestaoEmpresas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.plaf.basic.BasicInternalFrameTitlePane.SizeAction;

import java.util.ArrayList;
import java.util.Collections;
import java.io.*;
public class GestaoEmpresas implements Serializable{
    
    private Map<String,EmpresaTransportadora> empresasTransp;  //codUtilizador->Utilizador
   
    
    public GestaoEmpresas(){
        this.empresasTransp = new HashMap<>();
    }
    
    public GestaoEmpresas(Map<String,EmpresaTransportadora> empresas){
        setEmpresaTransportadora(empresas);
    }
    
    public GestaoEmpresas(GestaoEmpresas ge){
        this.empresasTransp= ge.getEmpresasTransp();
    }
    
    /**
     * 
     * Gets
     */
    public Map<String, EmpresaTransportadora> getEmpresasTransp(){
        //Map<String,EmpresaTransportadora > res =  new HashMap<>();
        //for(EmpresaTransportadora et: this.empresasTransp.values()){
        //    res.put(et.getCodEmp(),et.clone());
        //}
        return empresasTransp;
    }
    
    /**
     * Sets
     */
    public void setEmpresaTransportadora(Map<String,EmpresaTransportadora> empresasTransp){
        this.empresasTransp = new HashMap<>();
        empresasTransp.values().forEach(et->this.empresasTransp.put(et.getCodEmp(),et.clone()));
    }
    
    /**
     * Método clone()
     */
    
    public GestaoEmpresas clone(){
        return new GestaoEmpresas(this);
    }
    
    /**
     * Método equals()
     */
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GestaoEmpresas ge = (GestaoEmpresas) obj;
        return (
                ge.getEmpresasTransp().equals(this.empresasTransp));
    }
    
    
    //tostring
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append ("Gestão de Empresas Transportadoras: ").append (this.empresasTransp);
       
        return sb.toString();
    }
    
    
    public void adicionaEmpresa(EmpresaTransportadora novaEmpresa){
        empresasTransp.put(novaEmpresa.getCodEmp(),novaEmpresa);
    }
    
    public void removeEmpresa(String codEmp){
        empresasTransp.remove(codEmp);
    }
    
    
    public List<EmpresaTransportadora> top10Emp(){
        List<EmpresaTransportadora> novo = new ArrayList<>();
        Collections.sort(novo, new ComparatorKmsPercorridos());
        int size = 0;
        for (EmpresaTransportadora e: empresasTransp.values()){
           if(size<10){
            novo.add(e.clone());
            size += 1;
           }
           if(size == 10) break;
        }
        return novo;
    }

	public EmpresaTransportadora getEmpresa(String cod) {
        try{
            return this.empresasTransp.get(cod);
        }
        catch(NullPointerException e){
            return null;
        }
	}
}

