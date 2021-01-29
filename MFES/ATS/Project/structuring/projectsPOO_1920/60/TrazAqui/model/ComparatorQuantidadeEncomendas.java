package model;

import interfaces.IUser;

import java.util.AbstractMap.SimpleEntry;
import java.util.Comparator;

public class ComparatorQuantidadeEncomendas implements Comparator<SimpleEntry<IUser, Integer>> {

    public int compare(SimpleEntry<IUser, Integer> m1, SimpleEntry<IUser, Integer> m2) {
        if(m1.getValue().equals(m2.getValue()))
            return m1.getKey().compareTo(m2.getKey());

        else return m2.getValue() - m1.getValue();
    }

}
