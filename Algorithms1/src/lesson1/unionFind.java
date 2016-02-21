package lesson1;

import java.io.*;
import java.io.FileReader;

public class unionFind {

	private int[] id;
	
	//Union find 
	public unionFind(int N){
		id = new int[N];
		for (int i=0; i<N;i++)
			id[i]=i;
	}
	
	public boolean connected(int p, int q){
		return id[p]==id[q];
	}
	
	public void union(int p,int q){
		int idq=id[q];
		int idp=id[p];
		for (int i=0; i<id.length; i++){
			if (id[i]==idp)
				id[i]=idq;
		}
	}
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
				
		
		File f  = new File("testData.txt");
		FileReader fr = new FileReader("testData.txt");
		try (BufferedReader br = new BufferedReader(new FileReader(f))){
		String line = br.readLine();
		int N = Integer.parseInt(line);
		unionFind UF = new unionFind(N);
		
		while ((line=br.readLine())!=null){
			String[] ids=line.split(",");
			UF.union(Integer.parseInt(ids[0]),Integer.parseInt(ids[1]));
			

		}
		// Put tests of connectedness here
	}
		
		
		
		
		
	}

}
