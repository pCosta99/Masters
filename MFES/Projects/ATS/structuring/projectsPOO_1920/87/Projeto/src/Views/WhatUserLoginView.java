package Views;

public class WhatUserLoginView implements TrazAquiView {

    /**
     * Método que imprime uma mensagem pré definida.
     */
    @Override
    public void show() {
    }

    /**
     * Método que imprime um Objeto,
     * @param o Objeto a ser imprimido
     */
    @Override
    public void show(Object o) {
        System.out.print((String) o );
    }
}
