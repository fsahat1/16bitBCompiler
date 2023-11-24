#include <stdio.h>
enum cont {STOP,LEFTONLY,RIGHTONLY,LEFTTHENRIGHT,RIGHTTHENLEFT} ;
#define POSTPROCESS 1
#define LEFTISNUMBER 1
#define LEFTISVAR 2
#define RIGHTISELSE 1

struct status {
    enum cont nm;
    TreeElement * child1,* child2, *descendant;
    unsigned int extrastatus;
    int numdata;
};

int breaksp, continuesp,functionsp,casesp;
char breakstack[80][80];
char continuestack[80][80];
struct {
  char funname[80];
  int params;
}  functionstack[80];
struct {
  char casename[80];
  int labelcount;
} casestack[80];
void Emit(const char * st) {
    fprintf(outfile,"%s",st);
    fprintf(outfile,"\n");
}

void Emit(char * st) {
    fprintf(outfile,"%s",st);
    fprintf(outfile,"\n");
}

void IdentAddr(char * ia,TreeElement * N) {                                        
    if (N->GetIdentTableElement()->isglobal)  {
        Emit (" LOD R4,R15,R15");
        Emit (" WRD %s",N->GetIdentName());
        Emit (" ADD R10,R12,R4");
    }
    else {
        Emit (" LOD R4,R15,R15");
        Emit (" WRD %s",N->GetIdentName());
        Emit (" LOD R5,R15,R15");
        Emit (" WRD %s",N->GetIdentTableElement()->getOffset());
        Emit (" ADD R4,R4,R5");
        Emit (" ADD R10,R12,R4");
    }

}

bool LeftDescendantIsNumber (TreeElement * Node,int * num) {
    TreeElement * N1;
    bool result=false;
    N1=Node->GetLeft();
    if (N1!=NULL) {
        while (N1->GetCategory()>=CHAR || N1->GetCategory() == NUMBER
            || N1->GetCategory() == ONUMBER) { // not leaf
            if (N1->GetCategory() == NUMBER) {
                *num=N1->GetIntVal();
                result=true;
                break;
            }
            if (N1->GetCategory() == ONUMBER) {
                *num=N1->GetOctalVal();
                result=true;
                break;
            }
            if (N1->GetCategory() == CHAR) {
                *num=N1->GetCharVal();
                result=true;
                break;
            }
            if (N1->GetRight() != NULL)
                break;
            if (N1->GetLeft() == NULL)
                break;
            N1=N1->GetLeft();
        }
    }
    return result;
}

bool LeftDescendantIsIdentifer (TreeElement * Node,TreeElement * * identnode) {
    TreeElement * N1;
    bool result=false;
    N1=Node->GetLeft();
    if (N1!=NULL) {
        while (N1->GetCategory()>CHAR || N1->GetCategory() == IDENTIFIER ) { // not leaf
            if (N1->GetCategory() == IDENTIFIER) {
                *identnode=N1;
                result=true;
                break;
            }
            if (N1->GetRight() != NULL)
                break;
            if (N1->GetLeft() == NULL)
                break;
            N1=N1->GetLeft();
        }
    }
    return result;
}

struct status  BeforeCheck(TreeElement * Node,int pass) {
    TreeElement * Subnode,* identnode;
    enum cont nextmove;
    int num;
    struct status stat;
    if (Node==NULL) {
        stat.nm=STOP;
        return stat;
    }
    switch (Node->GetCategory()) {
    case UNKNOWN:
    case IDENTIFIER:
    case GOTOLABELT:
    case NUMBER:
    case ONUMBER:
    case STRING:
    case CHAR:
        nextmove=STOP;
        break;
    case EXTRNDECLARATION:
    case EXTRNDECLARATIONREST:
    case ARRAYSIZE:
        nextmove=RIGHTONLY;
        break;
   
    case CONTINUESTATEMENT:
    case BREAKSTATEMENT:
          nextmove=STOP;
        break;
    case GOTOLABEL:
        nextmove=STOP;
        break;
    case CASELABEL:
        if (LeftDescendantIsNumber (Node,&num)) {
            stat.extrastatus=LEFTISNUMBER;
            stat.numdata=num;
        }
        else
            stat.extrastatus=0;
        nextmove=STOP;
        break;
    case GOTOSTATEMENT:
        if (LeftDescendantIsIdentifer (Node,&identnode)) {
            stat.extrastatus=LEFTISVAR;
            stat.descendant=identnode;           
        }
        else
            stat.extrastatus=0;
        nextmove=STOP;
        break;       
    case IFSTATEMENT:
        Subnode=Node->GetRight();
        if (Subnode !=NULL) {
            if (Subnode->GetCategory() == ELSESTATEMENT) {
                stat.extrastatus=RIGHTISELSE;
                nextmove=LEFTTHENRIGHT;
            }
            else {
                stat.extrastatus=0;
                nextmove=LEFTTHENRIGHT;
            }
        }
        break;

    case SHIFTEXPRRESTRIGHT:
    case SHIFTEXPRRESTLEFT:
    case RELEXPRRESTGREATEREQUAL:
    case RELEXPRRESTLESSEQUAL:
    case RELEXPRRESTGREATER:
    case RELEXPRRESTLESS:
    case EQLEXPRRESTNOTEQ:
    case EQLEXPRRESTEQ:
    case ANDEXPRREST:
    case XOREXPRREST:
    case OREXPRREST:
    case MULTEXPRRESTDIV:
    case MULTEXPRRESTMUL:
    case ADDEXPRRESTSUB:
    case ADDEXPRRESTADD:
    case INITIALIZATOR:
        if (LeftDescendantIsNumber (Node,&num)) {
            stat.extrastatus=LEFTISNUMBER;
            stat.numdata=num;
            nextmove=RIGHTONLY;
        } else if (LeftDescendantIsIdentifer (Node,&identnode)) {
            stat.extrastatus=LEFTISVAR;
            stat.descendant=identnode;
            nextmove=RIGHTONLY;
        }
        else {
            stat.extrastatus=0;
            nextmove=LEFTTHENRIGHT;
        }
        break;

    case ASSIGNEXPRNOTEQUAL:
    case ASSIGNEXPREQUAL:
    case ASSIGNEXPRGREATEREQ:
    case ASSIGNEXPRGREATER:
    case ASSIGNEXPRLESSEQ:
    case ASSIGNEXPRLESS:
    case ASSIGNEXPRRSHIFT:
    case ASSIGNEXPRLSHIFT:
    case ASSIGNEXPROR:
    case ASSIGNEXPRXOR:
    case ASSIGNEXPRAND:
    case ASSIGNEXPRSUB:
    case ASSIGNEXPRADD:
    case ASSIGNEXPRMOD:
    case ASSIGNEXPRDIV:
    case ASSIGNEXPRMUL:
    case ASSIGNEXPRASSIGN:
        if (LeftDescendantIsIdentifer (Node,&identnode)) {
            stat.extrastatus=LEFTISVAR;
            stat.descendant=identnode;
            nextmove=RIGHTONLY;
        }
        else {
            stat.extrastatus=0;
            nextmove=RIGHTTHENLEFT;
        }
        break;

    case MINUSUNARYEXPR:
        if (LeftDescendantIsNumber (Node,&num)) {
            stat.extrastatus=LEFTISNUMBER;
            stat.numdata=num;
            nextmove=STOP;
        }
        else {
            stat.extrastatus=0;
            nextmove=LEFTONLY;
        }
        break;
    case ADDRUNARYEXPR:
        if (LeftDescendantIsIdentifer (Node,&identnode)) {
            stat.extrastatus=LEFTISVAR;
            stat.descendant=identnode;
            nextmove=STOP;
        }
        else {
            stat.extrastatus=0;
            nextmove=LEFTONLY;
        }
        break;

    case DECUNARYEXPR:
    case INCUNARYEXPR:
    case LOGNOTUNARYEXPR:
    case NOTUNARYEXPR:
    case PLUSUNARYEXPR:
    case POINTERUNARYEXPR:
        nextmove=LEFTONLY;
        break;

    case RETURNSTATEMENT:
        nextmove=LEFTONLY;
        break;
    case FUNCTIONBODY:
        nextmove=LEFTTHENRIGHT;
        break;

    case ARGEXPRLISTREST:
    case ARGEXPRLIST:
        nextmove=RIGHTTHENLEFT;
        break;
   
    case PRIMARY:
        Subnode=Node->GetLeft();
        if (Subnode->GetCategory()==EXPRESSION) {
            nextmove=LEFTTHENRIGHT;
            stat.child1=Subnode;
            stat.extrastatus=0;
        }
        else if (Subnode->GetCategory()==IDENT || Subnode->GetCategory()==CONSTVAL ) {
            stat.child1=Subnode;
            nextmove=STOP;
            stat.extrastatus=POSTPROCESS;
        }
        else {
            stat.child1=NULL;
            stat.extrastatus=0;
        }
        break;
    default:
        nextmove=LEFTTHENRIGHT;
    }
    stat.nm=nextmove;
    return stat;

}

void BeforeLeft(TreeElement * Node,int pass,struct status * stat) {
    char lab[80],statement[80];
    int locals;
    if (Node==NULL)
        return;
    if (pass==1) {
        switch (Node->GetCategory()) {
        case BPROGRAM:
            Emit (" section .data");
            break;
        }
    }
    else if (pass==2) {
        switch (Node->GetCategory()) {
        case BPROGRAM:
                                                                                        
            Emit (" section .text");
            Emit (" EQU R1,R1,R1");
            Emit (" SUB R11,R11,R11");
            Emit (" SUB R11,R11,R1");
            Emit (" SUB R12,R11,R1");
            breaksp=continuesp=functionsp=casesp=0;
            break;
        case SWITCHSTATEMENT:
            sprintf(lab,"_FIN%p",(void *) Node);
            breaksp++;
            strcpy(breakstack[breaksp],lab);
            casesp++;
            strcpy(casestack[casesp].casename,lab);
            break;
        case STATEMENTLABELS:
            if (casesp>0) {
              sprintf(statement,"_CASE%s_%d:",casestack[casesp].casename,casestack[casesp].labelcount);           
              Emit(statement);
            } 
            break;                   
        case WHILESTATEMENT:
            sprintf(lab,"_START%p",(void *) Node);
            sprintf(statement,"%s:",lab);
            Emit(statement);
            continuesp++;
            strcpy(continuestack[continuesp],lab);
            sprintf(lab,"_FIN%p",(void *) Node);
            breaksp++;
            strcpy(breakstack[breaksp],lab);
            break;

        case SHIFTEXPRRESTRIGHT:
        case SHIFTEXPRRESTLEFT:
        case RELEXPRRESTGREATEREQUAL:
        case RELEXPRRESTLESSEQUAL:
        case RELEXPRRESTGREATER:
        case RELEXPRRESTLESS:
        case EQLEXPRRESTNOTEQ:
        case EQLEXPRRESTEQ:
        case ANDEXPRREST:
        case XOREXPRREST:
        case OREXPRREST:
        case MULTEXPRRESTMOD:
        case MULTEXPRRESTDIV:
        case MULTEXPRRESTMUL:
        case ADDEXPRRESTSUB:
        case ADDEXPRRESTADD:
            if (stat->extrastatus==0)
                                                
                Emit (" SUB R12,R12,R1");
                Emit (" STO R2,R2,R12");
            break;
        case POSTFIXINDEX:
            if (stat->extrastatus==0)
                                               
                Emit (" SUB R12,R12,R1");
                Emit (" STO R6,R6,R12");
            break;
        case FUNCTIONDEFINITION:
            strcpy(lab,Node->GetParent()->GetLeft()->
              GetLeft()->GetIdentName());
            sprintf (statement,"WRD %s",lab);
            Emit(statement);
            sprintf (statement,"WRD %s:",lab);
            Emit(statement);
            break;           
        case FUNCTIONBODY:
                                                
            Emit (" SUB R12,R12,R1");
            Emit (" STO R8,R8,R12");
            Emit (" STO R6,R6,R11");
                                                
            Emit (" ORA R11,R12,R12");
            locals=Node->GetParent()->GetParent()->GetLeft()->
              GetLeft()->GetIdentTableElement()->getArraySize();
                                                
            Emit (" LOD R10,R15,R15");
            sprintf (statement," WRD %d",locals);
            Emit(statement);
            Emit (" SUB R12,R12,R10");
            break;
        case CONDEXPRREST:
                                                    
            Emit (" LTU R5,R2,R1");
            Emit (" LOD R10,R15,R15");
            sprintf(statement,"WRD _CONDELSE%p",(void *) Node);
            Emit(statement);
            Emit (" MIF R15,R5,R10");
            break;
           

        }
    }

}

void BeforeRight(TreeElement * Node,int pass,struct status * stat) {
    char statement[80],ident[80];
    TreeElement * N;
    if (Node==NULL)
        return;
    if (pass==1) {
        switch (Node->GetCategory()) {
        case ARRAYSIZE:
          N=Node->GetParent()->GetLeft()->GetLeft();
          strcpy(ident,N->GetIdentName());
          if (Node->GetLeft()!=NULL && Node->GetRight()==NULL) {
            N=Node->GetLeft();
            if (N != NULL)
                N=Node->GetLeft();
            if (N->GetCategory()==NUMBER) {
              sprintf(statement,"%s W 0",ident);
              Emit(statement);
              for(int temp=0; temp<N->GetIntVal())
              {
                sprintf(statement,"W 0");
                Emit(statement);
              }
            } 
          }                       
          if (Node->GetLeft()==NULL && Node->GetRight()==NULL) {
            sprintf(statement,"%s W 0",ident);
            Emit(statement);
          }             
          if (Node->GetLeft()==NULL && Node->GetRight()!=NULL) {
            sprintf(statement,"%s:",ident);
            Emit(statement);
          }   
          break;
        case INITIALIZATOR:
           if (stat->extrastatus==LEFTISNUMBER) {
               sprintf (statement," W %d",stat->numdata);
               Emit (statement);
           }
           if (stat->extrastatus==LEFTISVAR) {
               strcpy(ident,stat->descendant->GetIdentName());
               sprintf (statement," W %s",ident);
               Emit (statement);
           }       
           break;
        }
    }
    if (pass==2) {
        switch (Node->GetCategory()) {
        case IFSTATEMENT:
            if (stat->extrastatus==0) {                                    
                                                        
                Emit (" LTU R5,R2,R1");
                Emit (" LOD R10,R15,R15");
                sprintf(statement,"WRD _FIN%p",(void *) Node);
                Emit(statement);
                Emit (" MIF R15,R5,R10");
            }
            if (stat->extrastatus==RIGHTISELSE) {                          
                                                        
                Emit (" LTU R5,R2,R1");
                Emit (" LOD R10,R15,R15");
                sprintf(statement,"WRD _ELSE%p",(void *) Node);
                Emit(statement);
                Emit (" MIF R15,R5,R10");
            }
            break;
        case ELSESTATEMENT:
            Emit (" LOD R15,R15,R15");
            sprintf(statement,"WRD _FIN%p",(void *) Node->GetParent());     
            Emit(statement);
            sprintf(statement,"_ELSE%p:",(void *) Node->GetParent());        
            Emit(statement);
            break;
        case WHILESTATEMENT:                                               
                                                    
            Emit (" LTU R5,R2,R1");
            Emit (" LOD R10,R15,R15");
            sprintf(statement,"WRD _FIN%p",(void *) Node);
            Emit(statement);
            Emit (" MIF R15,R5,R10");
            break;
        case ADDEXPRRESTADD:
            if (stat->extrastatus==0) {
                                                                            
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" ADD R2,R2,R3");
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                            
                                                                            
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" ADD R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                            
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement,"WRD %s", ident);
                Emit (statement);
                Emit (" LOD R10,R9,R9");
                Emit (" ADD R2,R2,R10");
            }
            break;
        case ADDEXPRRESTSUB:
            if (stat->extrastatus==0) {
                                                                            
                Emit (" ORA R3,R2,R2");
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" SUB R2,R2,R3");

            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                            
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" SUB R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                            
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement,"WRD %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" SUB R2,R2,R10");
            }
            break;
        case MULTEXPRRESTMUL:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" MUL R2,R2,R3");
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                        
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" MUL R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement,"WRD %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" MUL R2,R2,R10");
            }
            break;
        case MULTEXPRRESTDIV:                                           //only whole numbers
            if (stat->extrastatus==0) {
                
                Emit (" ORA R3,R2,R2");
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
            
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");

            }
            if (stat->extrastatus==LEFTISNUMBER) {
                Emit (" LOD R3,R15,R15");
                sprintf (statement," WRD %d",stat->numdata);     
                Emit (statement);
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
            }
            if (stat->extrastatus==LEFTISVAR) {                     
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement,"WRD %s",ident);
                Emit (statement);
                Emit (" LOD R3,R3,R9");
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
            }
            break;
        case MULTEXPRRESTMOD:                                   
            if (stat->extrastatus==0) {
                Emit (" ORA R3,R2,R2");
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
            
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
                Emit (" ORA R2,R4,R4");

            }
            if (stat->extrastatus==LEFTISNUMBER) {                  
                Emit (" LOD R3,R15,R15");
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
                Emit (" ORA R2,R4,R4");
            }
            if (stat->extrastatus==LEFTISVAR) {                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R3,R3,R9");
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
                Emit (" ORA R2,R4,R4");
            }
            break;
        case OREXPRREST:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" ORA R2,R2,R3");

            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                        
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" ORA R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" ORA R2,R2,R10");
            }
            break;
        case XOREXPRREST:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" XOR R2,R2,R3");
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                        
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" XOR R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" XOR R2,R2,R10");
            }
            break;
        case ANDEXPRREST:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" AND R2,R2,R3");
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                        
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" AND R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" AND R2,R2,R10");
            }
            break;
        case EQLEXPRRESTEQ:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" EQU R2,R2,R3");
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                        
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" EQU R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                            
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" EQU R2,R2,R10");
            }
            break;
        case EQLEXPRRESTNOTEQ:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" EQU R2,R2,R3");
                Emit (" XOR R2,R2,R1");

            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                        
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" EQU R2,R2,R10");
                Emit (" XOR R2,R2,R1");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" EQU R2,R2,R10");
                Emit (" XOR R2,R2,R1");
            }
            break;
        case RELEXPRRESTLESS:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LTS R2,R2,R3");
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                        
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" LTS R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" LTS R2,R2,R10");
            }
            break;
        case RELEXPRRESTGREATER:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" GTS R2,R2,R3");

            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                          
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" GTS R2,R2,R10");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                        
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" GTS R2,R2,R10");
            }
            break;
        case RELEXPRRESTLESSEQUAL:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" GTS R2,R2,R3");
                Emit (" XOR R2,R2,R1");

            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                          
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" GTS R2,R2,R10");
                Emit (" XOR R2,R2,R1");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                            
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" GTS R2,R2,R10");
                Emit (" XOR R2,R2,R1");
            }
            break;
        case RELEXPRRESTGREATEREQUAL:
            if (stat->extrastatus==0) {
                                                                            
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LTS R2,R2,R3");
                Emit (" XOR R2,R2,R1");

            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                              
                Emit (" LOD R10,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" GTS R2,R2,R10");
                Emit (" XOR R2,R2,R1");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                            
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s", ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" GTS R2,R2,R10");
                Emit (" XOR R2,R2,R1");
            }
            break;
        case SHIFTEXPRRESTLEFT:
            if (stat->extrastatus==0) {
                                                                        
                Emit (" XOR R4,R4,R4");
                Emit (" ADD R4,R4,R2");
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 48");                                           //leftshift
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
                
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                Emit (" LOD R4,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 48");                                           
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                            
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s", ident);
                Emit (statement);
                Emit (" LOD R4,R4,R9");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 48");                                           //lijevo
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
            }
            break;
        case SHIFTEXPRRESTRIGHT:
            if (stat->extrastatus==0) {
                                                                            //NOVO
                Emit (" XOR R4,R4,R4");
                Emit (" ADD R4,R4,R2");
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 16");                                           //desno
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
            }
            if (stat->extrastatus==LEFTISNUMBER) {
                                                                            //NOVO
                Emit (" LOD R4,R15,R15"); 
                sprintf (statement," WRD %d",stat->numdata);
                Emit (statement);
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 16");                                           //desno micanje
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
            }
            if (stat->extrastatus==LEFTISVAR) {
                                                                            //NOVO
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s", ident);
                Emit (statement);
                Emit (" LOD R4,R4,R9");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 16");                                           //desno
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
            }
            break;
        case CONDEXPRREST:
            Emit (" LOD R15,R15,R15");
            sprintf(statement,"WRD _CONDEND%p",(void *) Node);             //NOVO
            Emit(statement);
            sprintf(statement,"_CONDELSE%p:",(void *) Node);               //NOVO
            Emit(statement);
            break;
        }
    }
}

void BeforeLateLeft(TreeElement * Node,int pass,struct status * stat) {
    if (Node==NULL)
        return;
    if (pass==2)
        switch (Node->GetCategory()) {

            switch (Node->GetCategory()) {
            case ASSIGNEXPRNOTEQUAL:
            case ASSIGNEXPREQUAL:
            case ASSIGNEXPRGREATEREQ:
            case ASSIGNEXPRGREATER:
            case ASSIGNEXPRLESSEQ:
            case ASSIGNEXPRLESS:
            case ASSIGNEXPRRSHIFT:
            case ASSIGNEXPRLSHIFT:
            case ASSIGNEXPROR:
            case ASSIGNEXPRXOR:
            case ASSIGNEXPRAND:
            case ASSIGNEXPRSUB:
            case ASSIGNEXPRADD:
            case ASSIGNEXPRMOD:
            case ASSIGNEXPRDIV:
            case ASSIGNEXPRMUL:
            case ASSIGNEXPRASSIGN:
                if (stat->extrastatus==0)
                                                                    //NOVO
                    Emit (" SUB R12,R12,R1");
                    Emit (" STO R2,R2,R12");
                break;
            }

        }
}

void AfterChildVisit(TreeElement * Node,int pass,struct status * stat) {
    char statement[80],ia[50],ident[80];
    TreeElement * N;
    int dt;
    if (Node==NULL)
        return;
    if (pass==1) {
        switch (Node->GetCategory()) {
        case PRIMARY:
            if (stat->extrastatus==POSTPROCESS) {
                N=stat->child1->GetLeft();
                if (N != NULL) {
                    if (N->GetCategory()==STRING) {
                        sprintf(statement," _SL%p db '%s',0",(void *) N,N->GetStringVal());
                        Emit (statement);
                    }
                }
            }
            break;
        case EXTRNDECLARATION:
        case EXTRNDECLARATIONREST:
            N=Node->GetLeft()->GetLeft();
            dt=N->GetIdentTableElement()->getDataType();
            if (dt !=FUNCTION) {
                if (hashSet->lookup(N->GetIdentName(), NULL) == NULL) {
                  sprintf(statement," extern %s",N->GetIdentName());
                  Emit (statement);
                } 
            }   
        break;
        }
    }


    if (pass==2) {
        switch (Node->GetCategory()) {
        case PRIMARY:
            N=stat->child1->GetLeft();
            if (N != NULL) {
                switch (N->GetCategory()) {
                case IDENTIFIER:
                    dt=N->GetIdentTableElement()->getDataType();
                    if (dt==INTEGER || dt==POINTER) {
                      IdentAddr(ia,N);
                                                                        //NOVO
                      Emit (" LOD R6,R15,R15");
                      sprintf(statement," WRD %s ",ia);
                      Emit (statement);
                      Emit (" LOD R2,R6,R6");
                    } else
                    if (dt==ARRAY) {
                                                                        //NOVO
                      Emit (" LOD R6,R15,R15");
                      IdentAddr(ia,N);
                      sprintf(statement," WRD %s ",ia);
                      Emit (statement);
                      } else
                    if (dt==FUNCTION) {
                      functionsp++;
                      functionstack[functionsp].params=0;                     
                      strcpy(functionstack[functionsp].funname,N->GetIdentName());
                      }
                       
                    break;
                case NUMBER:
                                                                        //NOVO
                    Emit (" LOD R2,R15,R15");                                                    
                    sprintf(statement," WRD %d ",N->GetIntVal());
                    Emit (statement);
                    break;
                case STRING:
                                                                        //NOVO
                    Emit (" LOD R2,R15,R15");                                                    
                    sprintf(statement," WRD _SL%p",(void *) N);
                    Emit (statement);
                    break;
                case CHAR:
                                                                        //NOVO
                    Emit (" LOD R2,R15,R15");                                                    
                    sprintf(statement," WRD %d ",N->GetCharVal());
                    Emit (statement);

                    break;
                case ONUMBER:
                                                                        //NOVO
                    Emit (" LOD R2,R15,R15");                                                    
                    sprintf(statement," WRD %d ",N->GetOctalVal());
                    Emit (statement);
                    break;

                }

            }
            break;
        case EXTRNDECLARATION:
        case EXTRNDECLARATIONREST:
            N=Node->GetLeft()->GetLeft();
            dt=N->GetIdentTableElement()->getDataType();
            if (dt ==FUNCTION) {
                if (hashSet->lookup(N->GetIdentName(), NULL) == NULL) {
                  sprintf(statement," extern %s",N->GetIdentName());
                  Emit (statement);
                } 
            }   
            break;
        case IFSTATEMENT:
            sprintf(statement,"_FIN%p:",(void *) Node);
            Emit (statement);
            break;
        case SWITCHSTATEMENT:
            sprintf(statement,"_CASE%s_%d:",casestack[casesp].casename,casestack[casesp].labelcount);           
            Emit (statement);
            sprintf(statement,"_FIN%p:",(void *) Node);
            Emit (statement);
           
            breaksp--;
            casesp--;
            break;
        case BREAKSTATEMENT:
            Emit (" LOD R15,R15,R15");                                      //NOVO
            sprintf(statement," WRD %s", breakstack[breaksp]);
            Emit (statement);
            break;
        case CONTINUESTATEMENT:
            Emit (" LOD R15,R15,R15");                                      //NOVO
            sprintf(statement," WRD %s", continuestack[continuesp]);
            Emit (statement);
            break;
        case CASELABEL:                                                     //NOVO           
            
            Emit (" LOD R9,R15,R15"); 
            sprintf(statement," WRD _DOCASE%s_%d",casestack[casesp].casename,casestack[casesp].labelcount);           
            Emit(statement);
            Emit (" LOD R8,R15,R15"); 
            sprintf(statement," WRD %d",stat->numdata);
            Emit(statement);
            Emit (" EQU R5,R2,R8");
            Emit (" MIF R15,R5,R9");
            break;       
        case STATEMENTLABELS:
            if (casesp>0) {
              Emit (" LOD R15,R15,R15");                                    //NOVO
              sprintf(statement," WRD _CASE%s_%d",casestack[casesp].casename,casestack[casesp].labelcount+1);           
              Emit(statement);
              sprintf(statement,"_DOCASE%s_%d:",casestack[casesp].casename,casestack[casesp].labelcount);           
              Emit(statement);
              casestack[casesp].labelcount++;             
            } 
            break;       
           
        case GOTOLABEL:
            sprintf(statement,"_%s:",Node->GetLeft()->GetGotoLabelName());
            Emit(statement);
             break;

        case GOTOSTATEMENT:                                                     //NOVO
            if (stat->extrastatus==LEFTISVAR) {
                Emit (" LOD R15,R15,R15");
                sprintf(statement," WRD _%s",stat->descendant->GetIdentName());
                Emit(statement);
             }
            break;           
        case WHILESTATEMENT:                                                    //NOVO
            Emit (" LOD R15,R15,R15");
            sprintf(statement," WRD _START%p",(void *) Node);
            Emit(statement);
            sprintf(statement," _FIN%p:",(void *) Node);
            Emit(statement);
            breaksp--;
            continuesp--;
            break;
        case RETURNSTATEMENT:                                                  //NOVO
            Emit (" LOD R8,R8,R12");
            Emit (" ADD R12,R12,R1");
            Emit (" ORA R15,R8,R8");
            break;
        case POSTFIXCALL:                                                      //NOVO
            Emit (" LOD R9,R15,R15");
            sprintf(statement," WRD %s",functionstack[functionsp].funname);
            Emit(statement);
            Emit (" MAJ R8,R15,R9");
            
            functionsp--;
            break;
           
        case ASSIGNEXPRASSIGN:
            if (stat->extrastatus==0) {                            //NOVO
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" STO R10,R6,R2");

            }
            if (stat->extrastatus==LEFTISVAR) {                    //NOVO
                IdentAddr(ident,stat->descendant);
                Emit (" LOD R9,R15,R15");
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" STO R10,R9,R2");
            }
            break;
        case ASSIGNEXPRMUL:
            if (stat->extrastatus==0) {                             //NOVO
                
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                
                Emit (" LOD R10,R6,R6");
                Emit (" MUL R2,R2,R10");
                Emit (" STO R10,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                     //NOVO
                IdentAddr(ident,stat->descendant);
                Emit (" LOD R10,R15,R15");
                sprintf (statement,"WRD %s",ident);
                Emit (statement);
                Emit (" MUL R2,R2,R10");
                Emit (" STO R10,R2,R10");
            }
            break;
        case ASSIGNEXPRDIV:                                     //NOVO
            if (stat->extrastatus==0) {
            
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
            
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
                
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                 //NOVO
                Emit (" XOR R3,R3,R3");
                Emit (" ADD R3,R3,R2");
                Emit (" LOD R9,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s",ident);
                Emit (statement);
                Emit (" LOD R2,R2,R9");

                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
                
                Emit (" LOD R10,R15,R15");
                sprintf (statement,"WRD %s",ident);
                Emit (statement);
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRMOD:                                     //NOVO
            if (stat->extrastatus==0) {
                
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
            
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
                Emit (" ORA R2,R4,R4");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                 //NOVO
                Emit (" XOR R3,R3,R3");
                Emit (" ADD R3,R3,R2");
                
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement,"%s",ident);
                Emit (statement);
                Emit (" LOD R2,R2,R10");
                
                Emit (" XOR R4,R4,R4");
                Emit (" LOD R8,R15,R15");
                Emit (" WRD 16");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 32767");
                Emit (" ORA R5,R15,R15");

                Emit (" GTU R9,R2,R10");
                Emit (" ADD R2,R2,R2");

                Emit (" ADD R4,R4,R4");
                Emit (" ADD R4,R4,R9");

                Emit (" LTU R9,R4,R3");
                Emit (" ADD R15,R15,R9");
                Emit (" SUB R4,R4,R3");

                Emit (" ADD R2,R2,R9");
                Emit (" SUB R8,R8,R1");
                Emit (" LTU R9,R8,R1");
                Emit (" XOR R9,R9,R1");
                Emit (" ADD R15,R15,R9");
                Emit (" ORA R15,R5,R5");
                Emit (" ORA R2,R4,R4");
                Emit (" LOD R10,R15,R15");
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRADD:
            if (stat->extrastatus==0) {                             //NOVO                                  //ISPOD OK

                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" ADD R2,R2,R10");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                     //NOVO
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R9,R10,R10");
                Emit (" ADD R2,R2,R9");
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRSUB:
            if (stat->extrastatus==0) {                             //NOVO
                
                Emit (" LOD R3,R3,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" SUB R2,R2,R3");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                     //NOVO
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R9,R10,R10");
                Emit (" SUB R2,R2,R9");
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRAND:
            if (stat->extrastatus==0) {                             //NOVO
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" AND R2,R2,R10");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                     //NOVO
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R9,R10,R10");
                Emit (" AND R2,R2,R9");
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRXOR:                                         //NOVO
            if (stat->extrastatus==0) {
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" XOR R2,R2,R10");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                     //NOVO
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R9,R10,R10");
                Emit (" XOR R2,R2,R9");
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPROR:                                          //NOVO
            if (stat->extrastatus==0) {
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" ORA R2,R2,R10");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                     //NOVO
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R9,R10,R10");
                Emit (" ORA R2,R2,R9");
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRLSHIFT:
            if (stat->extrastatus==0) {
                Emit (" XOR R4,R4,R4");
                Emit (" ADD R4,R4,R2");
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 48");                                           //lijevo micanje
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                             //NOVO
                
                Emit (" XOR R4,R4,R4");
                Emit (" ADD R4,R4,R2");
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s",ident);
                Emit (statement);
                Emit (" LOD R2,R2,R10");
                Emit (" LOD R9,R15,R15");
                Emit (" WRD 48");                                           //lijevo micanje
                Emit (" ORA R4,R4,R9");
                Emit (" SHR R2,R2,R4");
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRRSHIFT:                                              //NOVO
            if (stat->extrastatus==0) {
                Emit (" XOR R4,R4,R4");
                Emit (" ADD R4,R4,R2");
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R15,R15");
                Emit (" WRD 16");                                           //desno micanje
                Emit (" ORA R4,R4,R10");
                Emit (" SHR R2,R2,R4");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {                             //NOVO
                Emit (" XOR R4,R4,R4");
                Emit (" ADD R4,R4,R2");
                Emit (" LOD R10,R15,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," %s",ident);
                Emit (statement);
                Emit (" LOD R2,R2,R10");
                Emit (" LOD R9,R15,R15");
                Emit (" WRD 16");                                           //desno micanje
                Emit (" ORA R4,R4,R9");
                Emit (" SHR R2,R2,R4");
                Emit (" STO R2,R2,R10");
            }
            break;
        case ASSIGNEXPRLESS:
            if (stat->extrastatus==0) {                     //NOVO
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" LTS R2,R6,R2");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {             //NOVO
                Emit (" LOD R9,R9,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement,"%s",ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" LTS R2,R6,R2");
                Emit (" STO R2,R2,R9");
            }
            break;
        case ASSIGNEXPRLESSEQ:                              //NOVO
            if (stat->extrastatus==0) {
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" GTU R2,R6,R2");
                Emit (" XOR R2,R2,R1");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {             //NOVO
                Emit (" LOD R9,R9,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" GTU R2,R6,R2");
                Emit (" XOR R2,R2,R1");
                Emit (" STO R2,R2,R6");
            }
            break;
        case ASSIGNEXPRGREATER:                             //NOVO
            if (stat->extrastatus==0) {
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" GTS R2,R6,R2");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {             //NOVO
                Emit (" LOD R9,R9,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" GTS R2,R6,R2");
                Emit (" STO R2,R2,R9");
            }
            break;
        case ASSIGNEXPRGREATEREQ:
            if (stat->extrastatus==0) {                     //NOVO
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" LTU R2,R6,R2");
                Emit (" XOR R2,R2,R1");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {             //NOVO
                Emit (" LOD R9,R9,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" LTU R2,R6,R2");
                Emit (" XOR R2,R2,R1");
                Emit (" STO R2,R2,R6");
            }
            break;
        case ASSIGNEXPREQUAL:
            if (stat->extrastatus==0) {                     //NOVO
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" EQU R2,R6,R2");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {             //NOVO
                Emit (" LOD R9,R9,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" EQU R2,R6,R2");
                Emit (" STO R2,R2,R9");
            }
            break;
        case ASSIGNEXPRNOTEQUAL:
            if (stat->extrastatus==0) {                 //NOVO
                Emit (" LOD R2,R2,R12");
                Emit (" ADD R12,R12,R1");
                Emit (" LOD R10,R10,R6");
                Emit (" EQU R2,R6,R2");
                Emit (" XOR R2,R2,R1");
                Emit (" STO R2,R2,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {         //NOVO
                Emit (" LOD R9,R9,R15");
                IdentAddr(ident,stat->descendant);
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R10,R10,R9");
                Emit (" EQU R2,R6,R2");
                Emit (" XOR R2,R2,R1");
                Emit (" STO R2,R2,R9");
            }
            break;
        case ADDRUNARYEXPR:
            if (stat->extrastatus==0) {                    //NOVO
                Emit (" ORA R2,R6,R6");
            }
            if (stat->extrastatus==LEFTISVAR) {             //NOVO
                IdentAddr(ident,stat->descendant);
                Emit (" LOD R10,R15,R15");
                sprintf (statement," WRD %s",ident);
                Emit (statement);
                Emit (" LOD R2,R10,R10");
            }
            break;
        case MINUSUNARYEXPR:
            if (stat->extrastatus==0) {                     //NOVO
                Emit (" ORA R3,R2,R2");
                Emit (" SUB R2,R2,R3");
                Emit (" SUB R2,R2,R3");
            }
            if (stat->extrastatus==LEFTISNUMBER) {          //NOVO
                Emit (" LOD R2,R15,R15");
                sprintf (statement," WRD -%d",stat->numdata);
                Emit (statement);
            }
            break;
        case POINTERUNARYEXPR:                              //NOVO
            Emit (" XOR R6,R6,R6");
            Emit (" ADD R6,R6,R2");
            Emit (" LOD R2,R2,R6");
            break;
        case NOTUNARYEXPR:                                  //NOVO
            Emit (" LOD R3,R15,R15");
            Emit (" WRD -1");
            Emit (" XOR R2,R2,R3");
            break;
        case LOGNOTUNARYEXPR:                               //NOVO
            Emit (" XOR R2,R2,R1");
            Emit (" AND R2,R2,R1");
            break;
        case INCUNARYEXPR:                                 //NOVO
            Emit (" LOD R10,R10,R6");
            Emit (" ADD R10,R10,R1");
            Emit (" STO R10,R10,R6");
            Emit (" LOD R2,R2,R6");
            break;
        case DECUNARYEXPR:                                 //NOVO
            Emit (" LOD R10,R10,R6");
            Emit (" SUB R10,R10,R1");
            Emit (" STO R10,R10,R6");
            Emit (" LOD R2,R2,R6");
            break;
        case POSTFIXINDEX:                                  //NOVO
            if (stat->extrastatus==0) {
                Emit (" LOD R6,R6,R12");
                Emit (" ADD R12,R12,R1");
                
                Emit (" SUB R10,R6,R2");
                Emit (" LOD R6,R6,R10");
                Emit (" LOD R2,R2,R6");
            }
            break;
        case POSTFIXINC:                                    //NOVO
            Emit (" LOD R2,R2,R6");
            Emit (" LOD R10,R10,R6");
            Emit (" ADD R10,R10,R1");
            Emit (" STO R10,R10,R6");
            break;
        case POSTFIXDEC:                                    //NOVO
            Emit (" LOD R2,R2,R6");
            Emit (" LOD R10,R10,R6");
            Emit (" SUB R10,R10,R1");
            Emit (" STO R10,R10,R6");

            break;
        case ARGEXPRLISTREST:
        case ARGEXPRLIST:                                  //NOVO
            functionstack[functionsp].params++;
            Emit (" SUB R12,R12,R1");
            Emit (" STO R2,R2,R12");
            break;
        case FUNCTIONBODY:                                 //NOVO
            Emit (" ORA R12,R11,R11");
            Emit (" LOD R2,R2,R11");
            Emit (" ADD R11,R11,R1");

            Emit (" LOD R8,R8,R12");
            Emit (" ADD R12,R12,R1");
            Emit (" ORA R15,R8,R8");
            break;
        case CONDEXPRREST:
            sprintf(statement," _CONDEND%p",(void *) Node);
            Emit(statement);
            break;
        }
    }
}

void Gen(TreeElement * Node,int pass) {
    enum cont nextmove;
    struct status stat;

    if (Node==NULL)
        return;

    stat=BeforeCheck(Node,pass);
    nextmove=stat.nm;
    if (nextmove==LEFTONLY || nextmove==LEFTTHENRIGHT) {
        BeforeLeft(Node,pass,&stat);
        Gen(Node->GetLeft(),pass);
    }
    if (nextmove==RIGHTONLY || nextmove==LEFTTHENRIGHT || nextmove==RIGHTTHENLEFT) {
        BeforeRight(Node,pass,&stat);
        Gen(Node->GetRight(),pass);
    }
    if (nextmove==RIGHTTHENLEFT) {
        BeforeLateLeft(Node,pass,&stat);
        Gen(Node->GetLeft(),pass);
    }
    AfterChildVisit(Node,pass,&stat);
}
