package model;

import interfaces.IEmpresa;

import java.util.AbstractMap;
import java.util.Comparator;

public class ComparatorQuantidadeKms implements Comparator<AbstractMap.SimpleEntry<IEmpresa, Double>> {

    public int compare(AbstractMap.SimpleEntry<IEmpresa,Double> m1, AbstractMap.SimpleEntry<IEmpresa,Double> m2) {
        if(m1.getValue().equals(m2.getValue()))
            return m1.getKey().compareTo(m2.getKey());

        else return (int) (m2.getValue() - m1.getValue());
    }
}
