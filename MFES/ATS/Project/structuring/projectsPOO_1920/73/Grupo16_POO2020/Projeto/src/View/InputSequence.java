package View;

import Helpers.IPair;
import View.IView;
import View.Window;

import java.util.*;

import View.Input;
import java.util.function.Function;
import java.util.AbstractMap.SimpleEntry;

import static View.InputHelper.*;


/**
 *
 * Esta classe é feita para facilitar o input de vários argumentos sequencias,
 *  fazendo handling de erros ou inputs incorretos, e caso pretenda, executar
 *  a função que requer esses argumentos
 *
 */

public class InputSequence {


    List<Object> args;
    //List<TYPE> getCommands;
    //FIXME - Pair
    List<SimpleEntry<TYPE, BetterPredicate>> getCommands;


    int currentCommand;
    boolean active; //indica se já foi cancelado ou não
    boolean ended; //indica se a recolha de inputs já acabou


    public InputSequence(List<SimpleEntry<TYPE, BetterPredicate>> sequence) {
        this.getCommands = new ArrayList<>(sequence);
        this.currentCommand = 0;
        this.args = new ArrayList<>(Collections.nCopies(sequence.size(),null));
        this.active = true;
        this.ended = false;
    }

    //TODO - mais contructors com 2, 3 argumentos (SimpleEntrys)
    public InputSequence() {
        this.getCommands = new ArrayList<>();
        this.args = new ArrayList<>(0);
        this.currentCommand = 0;
        this.active = true;
        this.ended = true; //FIXME
    }


    // -1 cancela, 0 volta atrás, else continuar
    private int applyNextCommand(IView v, Window w) {
        SimpleEntry<TYPE, BetterPredicate> command = getCommands.get(currentCommand);
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



    //todo - não vai ser void
    //todo -ser optional
    public Object exec(Function<List<Object>,Object> func) {
        if(this.active && this.ended)
            return func.apply(this.args);
        return null;
    }

    public List<Object> getArgs() {
        return this.args;
    }





}
