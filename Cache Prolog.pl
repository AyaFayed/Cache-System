pow(0,1).
pow(N,R):-
		   N>0,
		   N1 is N-1,
		   pow(N1,R1),
		   R is 2*R1.
   
convertBinToDechelp(0,_,0).
convertBinToDechelp(Bin,N,Dec):-
							   Bin>0,
							   Bin1 is Bin//10,
							   N1 is N+1,
							   X is Bin mod 10,
							   convertBinToDechelp(Bin1,N1,Dec1),
							   pow(N,Res),
							   Res1 is Res*X,
							   Dec is Res1+Dec1.
   
convertBinToDec(Bin,Dec):-
							convertBinToDechelp(Bin,0,Dec).
   
replaceIthItem(X,[_|T],0,[X|T]).
replaceIthItem(X,[H|T],N,[H|T1]):-
								N1 is N-1,
								replaceIthItem(X,T,N1,T1).




splitEvery(_,[],[]).
splitEvery(N,[H|T],R):-
						splitEveryhelper(N,[H|T],1,[],R).
splitEveryhelper(_,[],_,Acc,[Acc]):-
							Acc\=[].
splitEveryhelper(_,[],_,[],[]).		
splitEveryhelper(N,[H|T],C,Acc,R):-
									C \= N,
									append(Acc,[H],Acc1),
									C1 is C+1,
									splitEveryhelper(N,T,C1,Acc1,R).
splitEveryhelper(N,[H|T],N,Acc,R):-
									append(Acc,[H],Acc1),
									splitEveryhelper(N,T,1,[],R1),
									R = [Acc1|R1].


logBase2(N,R):-
				logBase2helper(N,0,R).
logBase2helper(1,Acc,Acc).
logBase2helper(N,Acc,R):-
						N > 1,
						X is N // 2,
						Acc1 is Acc +1,
						logBase2helper(X,Acc1,R).

getNumBits(_,fullyAssoc,_,0).

getNumBits(_,directMap,Cache,BitsNum):-
										length(Cache,X),
										logBase2(X,N),
										pow(N,X),
										BitsNum = N.
getNumBits(_,directMap,Cache,BitsNum):-
										length(Cache,X),
										logBase2(X,N),
										pow(N,Z),
										Z\=X,
										BitsNum is N+1.
										

getNumBits(NumOfSets,setAssoc,_,BitsNum):-
											logBase2(NumOfSets,X),
											pow(X,NumOfSets),
											BitsNum=X.
getNumBits(NumOfSets,setAssoc,_,BitsNum):-
											logBase2(NumOfSets,X),
											pow(X,Z),
											Z\=NumOfSets,
											BitsNum is X+1.										


fillZeros(String,0,String).
fillZeros(String,N,R):-
						N>0,
						N2 is N - 1,
						string_concat("0", String,Re),
						fillZeros(Re,N2,R).

						
						
getDataFromCache(StringAddress,L,Data,0,directMap,BitsNum):-
						getNumBits(_,directMap,L,BitsNum),
						atom_number(StringAddress,Bin),
						convertAddress(Bin,BitsNum,Tag1,Idx1,directMap),
						convertBinToDec(Idx1,Idx),
						nth0(Idx,L,item(tag(Tag),data(Data),1,_)),
						atom_number(Tag,Tag2),
						convertBinToDec(Tag1,T),
						convertBinToDec(Tag2,T).
getDataFromCache(Address,R,Data,HopsNum,setAssoc,Numsets):-
														length(R,CacheSize),
														cielDiv(CacheSize,Numsets,SetSize),
														splitEvery(SetSize,R,L),
														atom_number(Address,NewNum),
														convertAddress(NewNum,Numsets,Tag,Idx,setAssoc),
														convertBinToDec(Idx,IdxDec),
														nth0(IdxDec,L,Zitems),
														searchInSet(Zitems,Tag,Data,0,HopsNum).
														

getNumb(0,0).
getNumb(1,10).
getNumb(BitsNum,R):-
					BitsNum>1,
					B1 is BitsNum-1,
					getNumb(B1,R1),
					R is 10*R1.
    

convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
					getNumb(BitsNum,R),
					Tag is Bin//R,
					Idx is Bin mod R.
convertAddress(Bin,1,Bin,0,setAssoc).
convertAddress(Bin,SetNum,Tag,Idx,setAssoc):-
												getNumBits(SetNum,setAssoc,_,Res),
												Res \= 0,
												Tag is Bin // (10**Res),
												Idx is Bin mod (10**Res).						

inverseAddress(Tag,Idx,R,IdxBitNum):-
			atom_number(Tag1,Tag),
			atom_number(Idx1,Idx),
			numDigits(Idx,Y),
			N1 is IdxBitNum -Y,
			fillZeros(Idx1,N1,Idx2),
			string_concat(Tag1,Idx2,R1),
			atom_number(R1,R).
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,_):-
				getNumBits(_,directMap,OldCache,BitsNum),
				inverseAddress(Tag,Idx,AddressBin,BitsNum),
				convertBinToDec(AddressBin,Address),
				nth0(Address,Mem,ItemData),
				convertTag(Tag,Tag2,BitsNum),
				convertBinToDec(Idx,Idx2),
				replaceIthItem(item(tag(Tag2),data(ItemData),1,0),OldCache,Idx2,NewCache).
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
															
															getNumBits(SetsNum,setAssoc,_,BitsNum),
															inverseAddress(Tag,Idx,AddressBin,BitsNum),
															convertBinToDec(AddressBin,Address),
															nth0(Address,Mem,ItemData),
															convertTag(Tag,Tag2,BitsNum),
															convertBinToDec(Idx,Idx2),
															length(OldCache,Len),
															cielDiv(Len,SetsNum,SubSize),
															splitEvery(SubSize,OldCache,TmpCache),
															nth0(Idx2,TmpCache,OldSub),
															updateSub(OldSub,TmpSub),
															getMaxIdx(TmpSub,IdxSub),
															replaceIthItem(item(tag(Tag2),data(ItemData),1,0),TmpSub,IdxSub,NewSub),
															replaceIthItem(NewSub,TmpCache,Idx2,NewCacheDiv),
															flatten(NewCacheDiv,NewCache).

															
replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
									convertBinToDec(Tag,Address),
									nth0(Address,Mem,ItemData),
									getMaxIdx(OldCache,Idx),
									convertTag(Tag,Tag2,0),
									updateSub(OldCache,TmpCache),
									replaceIthItem(item(tag(Tag2),data(ItemData),1,0),TmpCache,Idx,NewCache).
numDigits(0,1).
numDigits(1,1).	

				
numDigits(Digit,L):-
            Digit>1,
			Digit2 is Digit//10,
			numDigits(Digit2,L2),
			L is 1+L2.
convertTag(Tag,R,Bits):-
			numDigits(Tag,L),
			N is 6-L-Bits,
			atom_number(String,Tag),
			fillZeros(String,N,R).

			
%---------------------------------
cielDiv(X,Y,R):-
				R is X//Y,
				0 is X mod Y.
cielDiv(X,Y,R):-
				Z is X//Y,
				Re is X mod Y,
				Re \=0,
				R is Z+1.


															
searchInSet([item(tag(T1),data(_),_,_)|T],Tag,Data,HopsNum,MainHops):-
															atom_number(T1,MainTag),
															(MainTag \= Tag), 
															HopsNum1 is HopsNum +1,
															searchInSet(T,Tag,Data,HopsNum1,MainHops).

searchInSet([item(tag(T1),data(_),S,_)|T],Tag,Data,HopsNum,MainHops):-
															atom_number(T1,MainTag),
															(MainTag = Tag,S=0), 
															HopsNum1 is HopsNum +1,
															searchInSet(T,Tag,Data,HopsNum1,MainHops).
searchInSet([item(tag(T1),data(Data),S,_)|_],Tag,Data,HopsNum,HopsNum):-
															atom_number(T1,MainTag),
															MainTag = Tag,
															S = 1.													

		



getMaxIdx(L,Idx):-
		findZero(L,0,Idx).
getMaxIdx(L,Idx):-
		\+findZero(L,0,_),
		findMax(L,0,0,0,Idx).
		
findZero([item(_,_,0,_)|_],X,X).
findZero([item(_,_,1,_)|T],Acc,R):-
				Acc2 is Acc+1,
				findZero(T,Acc2,R).

findMax([],_,_,X,X).
findMax([item(_,_,1,E)|T],Curr,MaxSoFar,MaxIdx,Res):-
				E=<MaxSoFar,
				Curr2 is Curr +1,
				findMax(T,Curr2,MaxSoFar,MaxIdx,Res).
findMax([item(_,_,1,E)|T],Curr,MaxSoFar,_,Res):-
					E>MaxSoFar,
					Curr2 is Curr+1,
					findMax(T,Curr2,E,Curr,Res).

updateSub([],[]).
updateSub([item(W,X,1,Z)|T],[item(W,X,1,U)|T2]):-
											U is Z+1,
											updateSub(T,T2).
updateSub([item(W,X,0,Z)|T],[item(W,X,0,Z)|T2]):-
											updateSub(T,T2).			
											
getData(StringAddress,OldCache,_,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],_,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
getNumBits(NumOfSets,Type,OldCache,BitsNum),
(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,
Type,NumOfSets).
