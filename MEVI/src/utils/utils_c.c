/*
!==========================================================================================!
!==========================================================================================!
!                                                                                          !
! Copyright (C) 1991-2004  ; All Rights Reserved ; ATMET, LLC                              !
!                                                                                          !
! This file is free software; you can redistribute it and/or modify it under the           !
! terms of the GNU General Public License as published by the Free Software                !
! Foundation; either version 2 of the License, or (at your option) any later version.      !
!                                                                                          !
! This software is distributed in the hope that it will be useful, but WITHOUT ANY         !
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          !
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.                !
!                                                                                          !
! You should have received a copy of the GNU General Public License along with this        !
! program; if not, write to the Free Software Foundation, Inc.,                            !
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.                                 !
!==========================================================================================!
!==========================================================================================!
*/


/******************************************************************************************/
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include <dirent.h>
#include <string.h>
void filelist_c_( int *inum, int *indices, char *prefix, char *chario, int dirlen
                , int charlen ){

  struct dirent **nameout;
  char filestr[200],filestr_p[200],filestr_a[200];
  char dir[200],tmpdir[200];
  char fpref0[80],fpref1[80],fpref2[80];
  char c1[1];
  int n,m,i;
  int good,val,num,lastlen,index,tfound;
  int nval;
  char *token,*found;

  char *delim = "/\0";
  char *delim2 = "*\0";
  
  /*--------------------------------------------------------------------------------------*/
  /*     First thing is to split the prefix into  a directory and a file prefix.  To do   */
  /* this scan the string and detect the last  "/".                                       */
  /*--------------------------------------------------------------------------------------*/
  index=0;
  strcpy(tmpdir,"\0");

  /*----- Then we have an absolute path --------------------------------------------------*/
  if(strncmp(prefix,"/",1)==0){strcpy(tmpdir,"/\0");}

  
  token = strtok (prefix, delim);
  tfound=0;
  while (token !=  '\0') {
     tfound += 1;

     strcpy(dir,tmpdir);
     strcat(tmpdir,token);
     strcat(tmpdir,delim);
     strcpy(fpref0,token);

     /*----- Fetch next token ------------------------------------------------------------*/
     token = strtok('\0', delim);
  } /* end while */

  /*--------------------------------------------------------------------------------------*/
  /*    Now we have the string parsed into the file prefix and the directory. The next    */
  /* step is to break it into components and do a comparison with the directory contents. */
  /*--------------------------------------------------------------------------------------*/

  /*----- Find the star and break up file name. ------------------------------------------*/
  tfound=0;

  strcpy(fpref1,"\0");
  strcpy(fpref2,"\0");

  if(strncmp(fpref0,"*",1)==0){
    
    /*----- Try the first token ----------------------------------------------------------*/
    strcpy(fpref1,"");
    tfound=1;
    
    /*----- Try the next token -----------------------------------------------------------*/
    token = strtok('\0',delim2);
    if (token != '\0'){
      tfound=2;
      strcpy(fpref2,token);
    } /* end if */
  }else{
    /*----- Try the first token ----------------------------------------------------------*/
    token = strtok (fpref0, delim2);
    if (token != '\0'){
      tfound=1;
      strcpy(fpref1,token);
    } /* end if */
    
    /*----- Try the next token -----------------------------------------------------------*/
    token = strtok('\0',delim2);
    if (token != '\0'){
      tfound=2;
      strcpy(fpref2,token);
    }
    
  } /* end if */

  m=0;

  /*--------------------------------------------------------------------------------------*/
  /*    Scan in the directory contents                                                    */
  /*--------------------------------------------------------------------------------------*/
  num = scandir(dir, &nameout, 0, alphasort);
  
  strcpy(filestr,nameout[1]->d_name);
  
  /*----- Set the string vector to null --------------------------------------------------*/
  strcpy(chario,"");
  
  /*--------------------------------------------------------------------------------------*/
  /*    Test if there are any entries, there should be at least two if the command was    */
  /* succesful.                                                                           */
  /*--------------------------------------------------------------------------------------*/
  if (num < 0){
    perror("scandir");
  }else if(num == 0){
    /*----- Only one entry? I don't think it's possible, but  deallocate it anyway... ----*/
    free(nameout[0]);
  }else if(num == 1){
    /*----- The directory was scanned, but there is nothing in it, just the . and .. -----*/
    free(nameout[0]);
    free(nameout[1]);
  }else{
    /*------------------------------------------------------------------------------------*/
    /*    So there is something in here besides the two base dirs...                      */
    /*------------------------------------------------------------------------------------*/
    /*----- Set the first index of the string to 1. --------------------------------------*/
    indices[0]=1;

    /*----- n = 0,1 are the base dirs, skip them -----------------------------------------*/
    for(n=2;n<num;n++){
      /*----- Copy the first file returned by scandir to filestr -------------------------*/
      strcpy(filestr,nameout[n]->d_name);
      
      /*----- Check for old files by looking for the ~ character -------------------------*/
      good=0;
      for(i=0;i<strlen(filestr);i++){
        val=strcmp("~",&filestr[i]);
        if(val==0){good=1;}
      } /* end for */

      /*----------------------------------------------------------------------------------*/
      /*    If the entry is not obsolete then compare it to the file prefix strings       */
      /* fpref1 and fpref2.                                                               */
      /*----------------------------------------------------------------------------------*/
      if(good<1){
        /* Compare to prefix 1, fpref1 */
        val=0;
        if (tfound>0) {
          val=-1;
          val=strncmp(fpref1,filestr,strlen(fpref1));

          /*----- Now compare the end of the string --------------------------------------*/
          if (tfound>1 & val==0) {
            val=-1;

            /*----------------------------------------------------------------------------*/
            /*    Set a new variable filestr_p, to be the remaining file string that has  */
            /* not been compared yet.                                                     */
            /*----------------------------------------------------------------------------*/
            strcpy(filestr_p,"\0");
            strcat(filestr_p,&filestr[  strlen(filestr)-strlen(fpref2)   ]); 
            strcat(filestr_p,"\0");

            /*----- Search the new string for the second search prefix -------------------*/
            val=strncmp(fpref2,filestr_p,strlen(fpref2));
          } /* end if */
        } /*end if */

        if (val==0) {
          
          /*------------------------------------------------------------------------------*/
          /*    If val==0, then we matched all necessary parts of the file prefix with    */
          /* the current file. Add this strings name to the vector                        */
          /*------------------------------------------------------------------------------*/
          /*----- Add the vector and give it back its direcotry path ---------------------*/
          strcpy( filestr_a,"\0");
          strcat( filestr_a,dir);
          strcat( filestr_a,filestr);
          strcat( filestr_a,"\0");
          strcat( chario,filestr_a);

          /*----- And give it a position index in the vector -----------------------------*/
          if(m>0){indices[m]=indices[m-1]+lastlen;}
          lastlen=strlen(filestr_a);
          m++;
        } /* end if */
      } /* end if */
      free(nameout[n]);

    } /* end for */
    //    m=m-1;
    
  }  /* end if */
  
  indices[m]=indices[m-1]+lastlen;

  /*----- Append a null character to the end of the string -------------------------------*/
  strcat( chario,"\0");
  
  /*----- Return the number of entries ---------------------------------------------------*/
  *inum=m;

  /*----- Release the scratch sting structure from memory --------------------------------*/
  free(nameout);
    


} /* end void */
