package lesson1;

import java.io.*;
import java.io.FileReader;
import java.util.*;

public class unionFind {

	private int[] id;
	private int[] sz;
	
	//Union find 
	//Always linear time.
	public unionFind(int N){
		id = new int[N];
		sz = new int[N];
		for (int i=0; i<N;i++){
			id[i]=i;
			sz[i]=1;
		}
	}
	
	public void printArr(int[] t){
		for (int i=0;i<t.length;i++){
			System.out.print(t[i]+"("+sz[i]+"),");
		}
	}
	
	//Quick find
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
	//quick union
	//degenerate case of tall trees makes this in the worst case linear time.
	public void quickUnion(int p,int q){
		id[root(p)]=id[root(q)];
	}
	
	public boolean unionConnected(int p,int q){
		return (root(p)==root(q));
	}
	
	private int root(int i){
		while (i!=id[i]) i=id[i];
		return i;
		
	}
	
	public void weightedUnion(int p,int q){
		if (p==q) return;
		int i = root(p);
		int j=root(q);
		//weighting by tree height NOT tree size as per video.
		if (sz[i]<sz[j])
		{
			id[i]=j;
			sz[i]=sz[j];
		}
		else if (sz[i]>sz[j]){
			id[j]=i;
			sz[j]=sz[i];
		}
		else {
			id[j]=i;
			sz[i]+=1;
			sz[j]+=1;
		}
		
	}
	
	
	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
	
		FileReader fr = new FileReader("testData.txt");
		try (BufferedReader br = new BufferedReader(fr)){
		String line = br.readLine();
		int N = Integer.parseInt(line);
		unionFind UF = new unionFind(N);
		System.out.print("N="+N+"\n");
		while ((line=br.readLine())!=null){
			String[] ids=line.split(",");
			//UF.union(Integer.parseInt(ids[0]),Integer.parseInt(ids[1]));
			//UF.quickUnion(Integer.parseInt(ids[0]),Integer.parseInt(ids[1]));
			UF.weightedUnion(Integer.parseInt(ids[0]),Integer.parseInt(ids[1]));
			System.out.print(ids[0]+","+ids[1]+"\n");
		}
		UF.printArr(UF.id);
		//quick union
		System.out.print("Connected 2,3 (asser true) "+UF.unionConnected(2,3));
		System.out.print("Connected 0,5 (asser false) "+UF.unionConnected(0,5));
		// Put tests of connectedness here
	}
			
	}

}
