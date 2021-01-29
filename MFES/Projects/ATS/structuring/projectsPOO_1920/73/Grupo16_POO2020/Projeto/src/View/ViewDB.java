package View;

import Helpers.ITriplet;
import Helpers.Triplet;
import Model.SystemUser;
import Model.Transportadora;

import java.util.*;
import java.util.function.Function;

import static View.InputHelper.isAny;
import static View.ViewHelper.*;

public class ViewDB {


    public static Panel banner() {
        Block b = new Block(
            new ArrayList<>(
                Arrays.asList(
                    newLine("████████╗██████╗  █████╗ ███████╗      █████╗  ██████╗ ██╗   ██╗██╗██╗ *BETA").color(7,GREEN_BOLD),
                    newLine("╚══██╔══╝██╔══██╗██╔══██╗╚══███╔╝     ██╔══██╗██╔═══██╗██║   ██║██║██║"),
                    newLine("   ██║   ██████╔╝███████║  ███╔╝█████╗███████║██║   ██║██║   ██║██║██║"),
                    newLine("   ██║   ██╔══██╗██╔══██║ ███╔╝ ╚════╝██╔══██║██║▄▄ ██║██║   ██║██║╚═╝"),
                    newLine("   ██║   ██║  ██║██║  ██║███████╗     ██║  ██║╚██████╔╝╚██████╔╝██║██╗"),
                    newLine("   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝     ╚═╝  ╚═╝ ╚══▀▀═╝  ╚═════╝ ╚═╝╚═╝")
                )
            ),0,1,0,1
        );
        return new Panel(b);
    }

    /*
    //Negativos contam apenas como espaço
    public static Map<Integer,String> loginOptions() {
        Map<Integer,String> map = new LinkedHashMap<>();
        map.put(1, "Cliente");
        map.put(2, "Loja");
        map.put(3, "Transportadora");
        map.put(4, "Voluntário");
        map.put(-1, "");
        map.put(5, "Registar");
        map.put(0, "Sair");
        map.put(10, "ADMIN");
        return map;
    }

     */



}
    /*
    public void setQuerieDB() {
        this.querieDB = new HashMap<>(Map.of(
                1,new Triplet<String, InputSequence, Function<List<Object>,Object>>(
                        "Produtos não comprados",
                        new InputSequence(),
                        this.Querie1),
                2,new Triplet<String, InputSequence, Function<List<Object>,Object>>(
                        "Vendas e Clientes distintos",
                        new InputSequence(Collections.singletonList(new AbstractMap.SimpleEntry<>(TYPE.INTEGER, isMonth()))),
                        this.Querie2),
                3,new Triplet<String, InputSequence, Function<List<Object>,Object>>(
                        "Informações de um Cliente (anual) ",
                        new InputSequence(Collections.singletonList(new AbstractMap.SimpleEntry<>(TYPE.STRING, isClient()))),
                        this.Querie3),
                4,new Triplet<String, InputSequence, Function<List<Object>,Object>>(
                        "Faturação de Produto em cada Mês",
                        new InputSequence(
                                Arrays.asList(
                                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isProduct()),
                                        new AbstractMap.SimpleEntry<>(TYPE.INTEGER, isMonth() )
                                )
                        ),

     */




