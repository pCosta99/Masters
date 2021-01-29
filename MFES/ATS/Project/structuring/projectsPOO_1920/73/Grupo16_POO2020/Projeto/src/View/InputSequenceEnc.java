package View;


import Helpers.IPair;
import Helpers.Triplet;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static View.InputHelper.*;

public class InputSequenceEnc extends InputSequence {


    List<Object> args;
    List<SimpleEntry<InputHelper.TYPE, BetterPredicate>> getCommands;
    List<String> labels;


    int currentCommand;
    boolean active; //indica se já foi cancelado ou não
    boolean ended; //indica se a recolha de inputs já acabou

    private Map<String, Triplet<String,Double,Double>> produtos;

    public InputSequenceEnc() {
        this.getCommands = new ArrayList<>();
        this.args = new ArrayList<>(0);
        this.currentCommand = 0;
        this.active = false;
        this.ended = false; //FIXME
        this.labels = new ArrayList<>();
        this.produtos = new LinkedHashMap<>();
    }

    public void setProdutos(Map<String, Triplet<String, Double, Double>> produtos) {
        this.produtos = produtos;
    }

    public BetterPredicate isValidProd() {
        return new BetterPredicate(
                p -> {
                    return this.produtos.containsKey(p);
                }, "Produto Válido"
        );
    }

    public BetterPredicate validQuantity() {
        return new BetterPredicate(
                p -> {
                    double value = Double.parseDouble((String) p);
                    double max = this.produtos.get(this.args.get(currentCommand-1)).getSecond();
                    return value <= max;
                }, "Quantidade Válida"
        );
    }



    public void addProdCommand() {
        this.getCommands.add(new SimpleEntry<>(InputHelper.TYPE.STRING,isValidProd()));
        this.args.add(null);
        this.labels.add("Cód. Produto");
        this.getCommands.add(new SimpleEntry<>(InputHelper.TYPE.STRING,validQuantity()));
        this.args.add(null);
        this.labels.add("Quantidade");
        this.active = true;
        this.ended = false;
    }

    // -1 cancela, 0 volta atrás, else continuar
    private int applyNextCommand(IView v, Window w) {
        SimpleEntry<InputHelper.TYPE, BetterPredicate> command = getCommands.get(currentCommand);
        switch(command.getKey()) {
            case INTEGER:
                IPair<Integer,Integer> resI = Input.readInt(command.getValue(),v, w,true);
                setArg(resI.getSecond());
                return resI.getFirst();
            case CHAR:
                break;
            case STRING:
                IPair<Integer,String> resS = Input.readString(command.getValue(),v,w);
                setArg(resS.getSecond());
                return resS.getFirst();
            default:
                break;
        }
        return 1;
    }


    public void nextCommand(IView v, Window w) {
        int res = this.applyNextCommand(v,w);
        switch (res) {
            case -1: //cancelado
                this.active = false;
                break;
            case 0: //atrás
                //TODO - é mesmo assim?
                args.set(currentCommand,null);
                this.currentCommand--;
                if(currentCommand < 0) this.active = false;
                else args.set(currentCommand,null);
                break;
            default:
                this.currentCommand++;
                if(currentCommand >= getCommands.size()) this.ended = true;
                break;
        }
    }

    public void gatherInputs(IView v, Window w){
        while(this.active && !this.ended) {
            nextCommand(v,w);
        }
    }

    private void setArg(Object arg) {
        args.set(currentCommand,arg);
    }

    public List<Object> getArgs() {
        return this.args;
    }

}
