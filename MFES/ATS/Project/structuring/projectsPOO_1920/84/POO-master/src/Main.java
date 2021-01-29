import controller.Controller;
import exceptions.ExcecaoLojaInexistente;
import exceptions.ExcecaoRegisto;
import exceptions.ExcecaoUserExistente;
import model.*;


import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) throws Exception {
        TrazAqui model = new TrazAqui();
        try {
            model = TrazAqui.read(".tmp");
            System.out.println("Anterior");
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Bem Vindo a TrazAqui");
            new Parse("bakFiles/logTeste.bak", model);

            Coordenadas coord = new Coordenadas(-23.1,43.2);
            List<Encomenda> reg = new ArrayList<>();
            List<Encomenda> pend = new ArrayList<>();
            Cliente c = new Cliente("1239","zat","csdvcsd","qwe","asdas",coord,reg,pend);
            List<Viagem> viagens = new ArrayList<>();
            Voluntario v = new Voluntario("3478","elena","asdasd","pass","morad",coord,3,false,true,viagens,20,3.4,50.7,reg,pend);
            List<Produto> prods = new ArrayList<>();
            ProdutoNormal prod = new ProdutoNormal(1.5,0.5,"ads",3,"p2345","caneta");
            prods.add(prod);
            //System.out.println(prods);
            //System.out.println(prods.toString());
            Encomenda enc = new Encomenda("e1234",c, LocalDateTime.now(),true,coord,1.2,prods);
            Encomenda enc2 = new Encomenda("e1434",c,LocalDateTime.of(2020,6,12,00,00),true,coord,1.2,prods);
            Encomenda enc3 = new Encomenda("e1235",c, LocalDateTime.now(),true,coord,1.2,prods);
            Encomenda enc4 = new Encomenda("e6789",c, LocalDateTime.now(),true,coord,1.2,prods);
            v.addEncomendaRegisto(enc);
            v.addEncomendaRegisto(enc2);
            reg.add(enc);
            EmpresaTransporte empresa = new EmpresaTransporte("adsd","1239",reg,1.2,4,coord,3.2);
            model.registaEmpresaTransporte(empresa);
            model.addUser(v);
            model.addUser(c);
            Voluntario novo = (Voluntario) model.getUtilizadores().get("3477");
            Voluntario novo1 = (Voluntario) model.getUtilizadores().get("4733");
            model.addEncomendaPendingAVoluntario(novo,enc);
            model.addEncomendaPendingAVoluntario(novo1,enc2);
            c.addEncomendaRegisto(enc2);
            //System.out.println(model.getEmpresas().get("1234").getRegistoEncomendas());



        }



        try { Thread.sleep(1000);} catch (Exception e) {}
        new Controller(model).run();
        try {
            model.save(".tmp");
        }
        catch (IOException ignored) {}



    }


}
