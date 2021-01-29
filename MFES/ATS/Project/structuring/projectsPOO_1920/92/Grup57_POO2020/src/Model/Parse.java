package Model;

import java.io.*;
import java.nio.*;
import java.util.*;
import Model.Account;
import Model.Encomenda;
import Model.Aceite;
import Model.ITrazAqui;

public class Parse {


	static public void parse(ITrazAqui trazAqui) {

		String line;
		String tokens[];

		try {
			final BufferedReader buf = new BufferedReader(new InputStreamReader(new FileInputStream("./logs.txt")));

			while((line = buf.readLine()) != null) {

				tokens = tokenize(line, ':');

				switch(tokens[0]) {

					case "Utilizador":
						parseUtilizador(tokens[1], trazAqui); // criar um Utilizador
						break;

					case "Loja":
						parseLoja(tokens[1], trazAqui); // criar uma Loja
						break;

					case "Voluntario":
						parseVoluntario(tokens[1], trazAqui); // criar um Voluntário
						break;

					case "Encomenda":
						parseEncomenda(tokens[1], trazAqui); // criar uma Encomenda
						break;

					case "Transportadora":
						parseTransportadora(tokens[1], trazAqui); // criar uma Transportadora
						break;

					case "Aceite":
						parseAceite(tokens[1], trazAqui); // criar uma Encomenda Aceite
						break;
					default:
						break;
				}
			}
			buf.close();
		}
		catch (final IOException e) {
			System.out.println("Ficheiro Inválido");
		}
	}


	static public void parseUtilizador (String utilizador, ITrazAqui trazAqui) {

		String tokens[] = tokenize(utilizador, ',');

		trazAqui.putLogin(tokens[0], tokens[0]);

		trazAqui.putUtilizador(tokens[0], new Utilizador(tokens[0], tokens[1]
				, new GPS(Double.parseDouble(tokens[2]), Double.parseDouble(tokens[3]))
				, tokens[0]));

	}


	static public void parseLoja(String loja, ITrazAqui trazAqui) {

		String tokens[] = tokenize(loja, ',');

		trazAqui.putLogin(tokens[0], tokens[0]);

		trazAqui.putLoja(tokens[0], new Loja(tokens[0], tokens[1]
				, new GPS(Double.parseDouble(tokens[2]), Double.parseDouble(tokens[3]))
				, tokens[0], true, new ArrayList<>()));
	}


	static public void parseVoluntario(String voluntario, ITrazAqui trazAqui) {

		String tokens[] = tokenize(voluntario, ',');

		trazAqui.putLogin(tokens[0], tokens[0]);

		trazAqui.putVoluntario(tokens[0], new Voluntario(tokens[0], tokens[1]
					  , new GPS(Double.parseDouble(tokens[2]), Double.parseDouble(tokens[3]))
					  , tokens[0]
				      , Double.parseDouble(tokens[4])
				      , new ArrayList<>(), true));
	}


	static public void parseEncomenda(String encomenda, ITrazAqui trazAqui) {

		String tokens[] = tokenize(encomenda, ',');

		List<LinhaEncomenda> set = new ArrayList<>();

		for(int i = 4; i < tokens.length; i += 4)
			set.add(new LinhaEncomenda(tokens[i], tokens[i + 1]
					, Double.parseDouble(tokens[i + 2])
					, Double.parseDouble(tokens[i + 3])));

		Encomenda e = new Encomenda(tokens[0], tokens[1], tokens[2], Double.parseDouble(tokens[3]), set);

		trazAqui.putEncomenda(tokens[0], e);
		trazAqui.adicionaEncomendaFilaDeEspera(tokens[2],e);
	}


	static public void parseTransportadora(String transportadora, ITrazAqui trazAqui) {

		String tokens[] = tokenize(transportadora, ',');

		trazAqui.putLogin(tokens[0], tokens[0]);

		trazAqui.putTransportadora(tokens[0], new Transportadora(tokens[0], tokens[1]
				, new GPS(Double.parseDouble(tokens[2]), Double.parseDouble(tokens[3]))
				, tokens[0], tokens[4]
				, Double.parseDouble(tokens[5]), Double.parseDouble(tokens[6])
				, new ArrayList<>(), true));
	}


	static public void parseAceite(String aceite, ITrazAqui trazAqui) {

		trazAqui.putAceite(new Aceite(aceite));
	}


	static public String[] tokenize(String string, char limite) {
		String[] aux = new String[(string.length() / 2) + 1];
		int c = 0;
		int i = 0;
		int j = string.indexOf(limite);

		while(j >= 0) {
			aux[c++] = string.substring(i, j);
			i = j + 1;
			j = string.indexOf(limite, i);
		}

		aux[c++] = string.substring(i);
		String[] tokens = new String[c];
		System.arraycopy(aux, 0, tokens, 0, c);
		return tokens;
	}
}