package Perfis;

import Auxiliares.GPS;
import Auxiliares.Viagem;
import Encomendas.Encomenda;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.chrono.ChronoLocalDateTime;
import java.util.ArrayList;


public abstract class Perfil implements Serializable , Comparable<Perfil> {

    /*
    Variaveis
     */

    private String codigoPerfil;
    private GPS coordenadas;
    private String nome, email, pass;
    private ArrayList<Encomenda> historico;

    /*
    Construtores
     */

    public Perfil(String codigoPerfil,GPS coordenadas, String nome, String email, String pass) {
        this.codigoPerfil = codigoPerfil;
        this.coordenadas = coordenadas;
        this.nome = nome;
        this.email = email;
        this.pass = pass;
        this.historico = new ArrayList<>();
    }

    public Perfil() {
        this.codigoPerfil = "";
        this.coordenadas = new GPS();
        this.nome = "";
        this.email = "";
        this.pass = "";
        this.historico = new ArrayList<>();
    }

    public Perfil(Perfil p) {
        this.codigoPerfil = p.getCodigoPerfil();
        this.coordenadas = p.getCoordenadas();
        this.nome = p.getNome();
        this.email = p.getEmail();
        this.pass = p.getPass();
        this.historico = p.getHistorico();
    }

    /*
    Gets e sets
     */

    public GPS getCoordenadas() {
        return coordenadas;
    }

    public void setCoordenadas(GPS coordenadas) {
        this.coordenadas = coordenadas;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPass() {
        return pass;
    }

    public void setPass(String pass) {
        this.pass = pass;
    }

    public String getCodigoPerfil() {
        return codigoPerfil;
    }

    public void setCodigoPerfil(String codigoPerfil) {
        this.codigoPerfil = codigoPerfil;
    }

    public ArrayList<Encomenda> getHistorico() {
        ArrayList<Encomenda> temp = new ArrayList<>();
        for (Encomenda encomenda : this.historico) temp.add(encomenda.clone());
        return temp;
    }

    public void setHistorico(ArrayList<Encomenda> hist) {
        this.historico = new ArrayList<>();
        for (Encomenda encomenda : hist) historico.add(encomenda.clone());
    }

    /*
    Metodos
     */

    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        if (!super.equals(object)) return false;
        Perfil perfil = (Perfil) object;
        return this.coordenadas.equals(perfil.getCoordenadas()) &&
                this.nome.equals(perfil.getNome()) &&
                this.email.equals(perfil.getEmail()) &&
                this.pass.equals(perfil.getPass()) &&
                this.historico.equals(perfil.getHistorico());
    }

    public abstract String toString();
    public abstract Perfil clone();

    // compara os perfis pelo tamanho do historico
    public int compareTamHistorico(Perfil x) {
        return Integer.compare(this.getHistorico().size(), x.getHistorico().size());
    }



    // Adiciona ao historico o ultimo pedido de encomenda
    public void adicionaAoHistorico(Encomenda e){
        this.historico.add(e);
    }

    // Ve o historico total
    public void printHistorico() {
        ArrayList<Encomenda> x = new ArrayList<>(this.getHistorico());
        StringBuilder sb = new StringBuilder();

        if (x.isEmpty()) { System.out.println("O seu histórico está vazio.\n"); }

        else {
            x.sort(Encomenda::compareTo); //Funciona mesmo abstrato
            for (Encomenda encomenda : x) {
                //if (encomenda.getEntregue().equals(true)) { ///
                sb.append("----------------------------------------------------------------------").append("\n")
                        .append(encomenda.toString())
                        .append("----------------------------------------------------------------------");
                //}
            }
            System.out.println("Lista de encomendas:\n");
            System.out.println(sb.toString());
        }
    }

    // Ve parte do historico
    public void historicoPeriodo(LocalDate inicio, LocalDate fim){

        ArrayList<Encomenda> x = new ArrayList<>();
        x.sort(Encomenda::compareTo);
        StringBuilder sb = new StringBuilder();

        // Se for para o mesmo dia
        if(inicio.isEqual(fim)) {
            for (Encomenda e : this.getHistorico()) {
                if ((LocalDate.from(e.getHoraPedido()).atStartOfDay()).isEqual(LocalDate.from(inicio).atStartOfDay())) {
                    x.add(e);
                }
            }
            if (x.isEmpty()) { System.out.println("Não houve encomendas neste período de tempo.\n\n"); }
            else {
                for (Encomenda encomenda : x) {
                    if (encomenda.getEntregue().equals(true)) {
                        sb.append("----------------------------------------------------------------------").append("\n")
                                .append(encomenda.toString())
                                .append("----------------------------------------------------------------------");
                    }
                }
                System.out.println("Lista de encomendas:\n");
                System.out.println(sb.toString());
            }
        }

        // Para dias diferentes
        else {
            if (inicio.isAfter(fim)) { System.out.println("As datas estão erradas, por favor tente novamente.\n"); }
            else {
                for (Encomenda e : this.getHistorico()) {
                    if (e.getHoraPedido().toLocalDate().isAfter(inicio) && e.getHoraPedido().toLocalDate().isBefore(fim)) {
                        x.add(e);
                    }
                }

                if (x.isEmpty()) {
                    System.out.println("Não houve encomendas neste período de tempo.\n\n");
                } else {
                    for (Encomenda encomenda : x) {
                        if (encomenda.getEntregue().equals(true)) {
                            sb.append("----------------------------------------------------------------------").append("\n")
                                    .append(encomenda.toString())
                                    .append("----------------------------------------------------------------------");
                        }
                    }
                    System.out.println("Lista de encomendas:\n");
                    System.out.println(sb.toString());
                }
            }
        }
    }

    public void atualizaHistorico (Encomenda encomenda, String codTransportadora,boolean entregar){
        if(codTransportadora==""){
                ArrayList<Encomenda> enc = this.getHistorico();
                enc.sort(Encomenda::compareTo);
                enc.add(encomenda);
                this.setHistorico(enc);
        }
        else{
            if(entregar == false){
            encomenda.setCodTransportadora(codTransportadora);
            ArrayList<Encomenda> enc = this.getHistorico();
            enc.sort(Encomenda::compareTo);
            enc.remove(enc.size()-1);
            enc.add(encomenda);
            this.setHistorico(enc);
            }
            else{
                encomenda.setEntregue(true);
                ArrayList<Encomenda> enc = this.getHistorico();
                enc.sort(Encomenda::compareTo);
                enc.remove(enc.size()-1);
                enc.add(encomenda);
                this.setHistorico(enc);
            }
        }
    }
    public int compareTo(Perfil o) {
        int codigoThis = Integer.parseInt(this.getCodigoPerfil().replaceFirst("([l]|[t]|[u]|[v])",""));
        int codigoOther= Integer.parseInt(o.getCodigoPerfil().replaceFirst("([l]|[t]|[u]|[v])",""));

        return Integer.compare(codigoThis,codigoOther);
    }

}
