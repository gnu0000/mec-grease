#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <GnuType.h>
#include <GnuDir.h>
#include <GnuReg.h>
#include <GnuArg.h>
#include <GnuStr.h>
#include <GnuFile.h>
#include <GnuMisc.h>


typedef struct _xlate
   {
   int   iFromLen;
   int   iToLen;
   int   iCount;
   PSZ   pszFrom;
   PSZ   pszTo;
   PRX   prx;
   struct _xlate *next;
   } XLATE;
typedef XLATE *PXLATE;


typedef struct
   {
   FILE  *fpIn;
   FILE  *fpOut;
   PSZ   pszIn;
   PSZ   pszOut;
   int   iLine;
   } FILEINFO;
typedef FILEINFO *PFILEINFO;



#define BUFF_SIZE  4096

PXLATE pXLATEHEAD = NULL;
PXLATE pXLATETAIL = NULL;

BOOL   bLISTONLY, bQUERY, bTEST, bOVERLAP;
BOOL   bREGEX, bSWAP, bCASE, bPARTIALS;
      
CHAR   aszBUFF [2][BUFF_SIZE];
UINT   uHOT = 0;

CHAR   szStar[] = "-\\|/";


/***************************************************************************/
/*                                                                         */
/* Fn's for loading the translation list from files and cmd line           */
/*                                                                         */
/***************************************************************************/


BOOL SplitCSVLine (PSZ pszFrom, PSZ pszTo, PSZ pszLine)
   {
   if (!StrGetCSVField (pszFrom, pszLine, 0))
      return 0;
   return StrGetCSVField (pszTo, pszLine, 1);
   }


BOOL SplitTABLine (PSZ pszFrom, PSZ pszTo, PSZ pszLine)
   {
   PSZ psz;

   if (!(psz = strchr (pszLine, '\t')))
         return 0;
   if (psz == pszLine)
         return 0;

   strncpy (pszFrom, pszLine, psz - pszLine);
   pszFrom[psz - pszLine] = '\0';
   strcpy (pszTo, psz +1);
   return TRUE;
   }

BOOL SplitDelimLine (PSZ pszFrom, PSZ pszTo, PSZ pszLine)
   {
   UCHAR cDelim;

   if (!pszLine || !*pszLine) 
      return FALSE;

   cDelim = *pszLine++;
   while (*pszLine && *pszLine != cDelim)
      *pszFrom++ = *pszLine++;
   *pszFrom = '\0';

   if (!*pszLine) 
      return FALSE;

   pszLine++;
   while (*pszLine && *pszLine != cDelim)
      *pszTo++ = *pszLine++;
   *pszTo = '\0';
   }

BOOL IsRegex (PSZ psz)
   {
   UINT i;
   PSZ  szTmp;

   if (!bREGEX)
      return FALSE;

   szTmp = "*?[]@+{}<>\\";

   for (i=0; i<11; i++)
      if (strchr (psz, szTmp[i]))
         return TRUE;
   return FALSE;
   }

BOOL AddXlate (PSZ pszLine, PSZ pszExt)
   {
   UCHAR  szFrom[256], szTo[256];
   BOOL   bRet;
   PXLATE pxlate;
   PSZ    p1, p2;

   if (pszLine && *pszLine && !stricmp (pszExt, ".CSV"))
      bRet = SplitCSVLine (szFrom, szTo, pszLine);
   else if (pszLine && *pszLine && !stricmp (pszExt, ".TAB"))
      bRet = SplitTABLine (szFrom, szTo, pszLine);
   else // normal delim file
      bRet = SplitDelimLine (szFrom, szTo, pszLine);

   if (!bRet)
      return FALSE;

   p1 = szFrom, p2 = szTo;
   if (bSWAP)
      p2 = szFrom, p1 = szTo;

   /*--- make new xlate node ---*/
   pxlate = calloc (1, sizeof (XLATE));
   pxlate->pszFrom  = strdup (p1);
   pxlate->iFromLen = strlen (p1);
   pxlate->pszTo    = strdup (p2);
   pxlate->iToLen   = strlen (p2);
   pxlate->iCount   = 0;
   pxlate->prx      = (IsRegex (p1) ? _regParseRegex (p1) : NULL);

   if (RegIsError ())
      Error ("Invalid regular expression: %s", RegIsError ());

   /*--- add new xlate node to list ---*/
   if (!pXLATETAIL)
      {
      pXLATEHEAD = pXLATETAIL = pxlate;
      }
   else
      {
      pXLATETAIL->next = pxlate;
      pXLATETAIL = pxlate;
      }
   return TRUE;
   }


void ReadXlateFile (PSZ pszFile)
   {
   char szLine[256];
   FILE *fp;

   if (!(fp = fopen (pszFile, "rt")))
      Error ("Unable to open Xlate file ", pszFile);

   while (FilReadLine (fp, szLine, ";\n", sizeof szLine) != -1)
      AddXlate (szLine, strrchr (pszFile, '.'));

   fclose (fp);
   }


/***************************************************************************/
/*                                                                         */
/* Fn's for actually doing the work on the target files                    */
/*                                                                         */
/***************************************************************************/

int BadChar (int c)
   {
   c = toupper (c);
   return  ((c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') ||
            (c == '_'));
   }


BOOL Isolated (PSZ pszLine, PSZ pszMatch, UINT uMatchLen)
   {
   UINT uLineLen;

   /*--- preceeding char ---*/
   if (pszMatch > pszLine && BadChar(pszMatch[-1]))
      return FALSE;

   /*--- trailing char ---*/
   uLineLen  = strlen (pszLine);
   if (uLineLen > (pszMatch - pszLine) + uMatchLen && BadChar(pszMatch[uMatchLen]))
      return FALSE;

   return TRUE;
   }


PSZ Mystrstr (PSZ pszLine, PSZ pszSub, BOOL bCaseSensitive)
   {
   PSZ   pszIn, pszOut, pszMatch;
   CHAR cStart;

   if (bCaseSensitive)
      return strstr (pszLine, pszSub);

   cStart = toupper (*pszSub);
   for (pszOut = pszLine; *pszOut; pszOut++)
      {
      if (toupper (*pszOut) != cStart)
         continue;
      pszMatch = pszSub+1;
      for (pszIn = pszOut+1; *pszMatch && *pszIn; pszIn++, pszMatch++)
         if (toupper (*pszIn) != toupper (*pszMatch))
            break;
      if (!*pszMatch)
         return pszOut;
      }
   return NULL;
   }


//UINT XlateLineStraight (PPSZ ppsz1, PPSZ ppsz2)
//   {
//   PSZ    pszSrc, pszDest, pszS, pszD, psz;
//   UINT   uLen, uStart, uXlates = 0;
//   PXLATE pxlate;
//
//   pszDest = *ppsz1;
//   pszSrc  = *ppsz2;
//
//   for (pxlate = pXLATEHEAD; pxlate; pxlate = pxlate->next)
//      {
//      uStart = 0;
//      pszS   = pszSrc;
//      pszD   = pszDest;
//
//      while (psz = Mystrstr (pszS, pxlate->pszFrom, bCASE))
//         {
//         if (!bPARTIALS && !Isolated (pszS, psz, pxlate->iFromLen))
//            {
//            uLen = psz - pszS + pxlate->iFromLen;
//            strncpy (pszD, pszS, uLen);
//            pszD += uLen;
//            pszS += uLen;
//            }
//         else
//            {
//            uXlates++;
//            uLen = psz - pszS;
//            strncpy (pszD, pszS, uLen);
//            pszD += uLen;
//            strcpy (pszD, pxlate->pszTo);
//            pszD += pxlate->iToLen;
//            pszS += uLen + pxlate->iFromLen;
//            }
//         }
//      strcat (pszD, pszS);
//
//      psz     = pszSrc;
//      pszSrc  = pszDest;
//      pszDest = psz;
//      }
//   *ppsz1 = pszSrc;
//   *ppsz2 = pszDest;
//   return uXlates++;
//   }



UINT Xlate (PXLATE pxlate)
   {
   UINT uXlates = 0;

   if (bREGEX && pxlate->prx)
      {
      UINT uStart  = 0;
      UINT uEnd    = BUFF_SIZE;

      while (_regFindReplace2 (aszBUFF[1-uHOT], aszBUFF[uHOT], 
                              pxlate->prx, pxlate->pszTo, &uStart, &uEnd))
         {
         uXlates++;
         uStart  = uEnd;
         uEnd    = BUFF_SIZE;
         uHOT    = 1-uHOT;
         }
      }
   else
      {
      PSZ  pszS   = aszBUFF[uHOT];
      PSZ  pszD   = aszBUFF[1-uHOT];
      PSZ  psz;
      UINT uLen;

      while (psz = Mystrstr (pszS, pxlate->pszFrom, bCASE))
         {
         if (!bPARTIALS && !Isolated (pszS, psz, pxlate->iFromLen))
            {
            uLen = psz - pszS + pxlate->iFromLen;
            strncpy (pszD, pszS, uLen);
            pszD += uLen;
            pszS += uLen;
            }
         else
            {
            uXlates++;
            uLen = psz - pszS;
            strncpy (pszD, pszS, uLen);
            pszD += uLen;
            strcpy (pszD, pxlate->pszTo);
            pszD += pxlate->iToLen;
            pszS += uLen + pxlate->iFromLen;
            }
         }
      if (uXlates)
         {
         strcat (pszD, pszS);
         uHOT = 1-uHOT;
         }
      }
   return uXlates;
   }


UINT XlateLine (void)
   {
   PXLATE pxlate;
   UINT   uXlates;

   uXlates = 0;
   for (pxlate = pXLATEHEAD; pxlate; pxlate = pxlate->next)
      uXlates += Xlate (pxlate);
   return uXlates;
   }


void XlateFile (PFILEINFO pfinfo, PULONG pulXlates, PULONG pulLines)
   {
   UINT z=0;

   *pulXlates = *pulLines = 0;
   putchar(szStar[z]);

   for (; fgets (aszBUFF[uHOT], BUFF_SIZE, pfinfo->fpIn); (*pulLines)++)
      {
      *pulXlates += XlateLine ();
      fputs (aszBUFF[uHOT], pfinfo->fpOut);

      if (!(*pulLines % 10))
         {
         putchar('\b');
         z = (z+1) % 4;
         putchar(szStar[z]);
         }
      }
   putchar('\b');
   }



BOOL ProcessFile (PSZ pszInFile, PULONG pulXlates, PULONG pulLines)
   {
   FILEINFO finfo;
   char     szOutFile[80];
   FILE     *fpIn, *fpOut;
   UINT     i, c;

   *pulLines = *pulXlates = 0;

   if (bLISTONLY)
      {
      printf (" %s\n", pszInFile);
      return FALSE;
      }
   if (!(fpIn = fopen (pszInFile, "r")))
      Error ("Unable to open Data file ", pszInFile);

   if (bTEST)
      strcpy (szOutFile, "NUL");
   else
      DirMakeFileName (szOutFile, NULL, pszInFile, ".@@@");

   if (!(fpOut = fopen (szOutFile, "w")))
      Error ("Unable to open Temp Data file ", szOutFile);

   finfo.fpIn   = fpIn;
   finfo.fpOut  = fpOut;
   finfo.pszIn  = pszInFile;  /*-- notice they aren't dupes --*/
   finfo.pszOut = szOutFile;
   finfo.iLine  = 0;

   if (bQUERY)
      {
      i = printf ("Process   : %s ? [Y/N]", pszInFile);
      c = getch ();
      putchar ('\r');
      for (; i>0; i--) putchar (' ');
      putchar ('\r');
      if (toupper (c) != 'Y')
         return FALSE;
      }
   printf ("Processing: %s ", pszInFile);
   XlateFile (&finfo, pulXlates, pulLines);

   for (i= strlen(pszInFile); i<40; i++)
      putchar ('.');
   printf (" %5.5ld Xlates, %5.5ld Lines\r           \n", *pulXlates, *pulLines);

   fclose (fpIn);
   fclose (fpOut);

   if (!bTEST)
      {
      unlink (pszInFile);
      rename (szOutFile, pszInFile);
      }
   return TRUE;
   }



void DoFileList (PSZ pszMatchSpec, BOOL bRecurse,
                 PUINT pusFiles, PULONG pulXlates, PULONG pulLines)
   {
   PFINFO pfo, pfoHead;
   CHAR   szPath[256], szMatchName[256], szSpec[256];
   ULONG  ulXlates, ulLines;

   DirSplitPath (szPath,      pszMatchSpec, DIR_DRIVE | DIR_DIR);
   DirSplitPath (szMatchName, pszMatchSpec, DIR_NAME  | DIR_EXT);

   /*--- Process Files first ---*/
   pfoHead = DirFindAll (pszMatchSpec, FILE_NORMAL);
   for (pfo = pfoHead; pfo; pfo = pfo->next)
      {
      sprintf (szSpec, "%s%s", szPath, pfo->szName);
      *pusFiles  += !!ProcessFile (szSpec, &ulXlates, &ulLines);
      *pulXlates += ulXlates;
      *pulLines  += ulLines;
      }
   DirFindAllCleanup (pfoHead);

   if (!bRecurse)
      return;

   /*--- now do subdirs ---*/
   sprintf (szSpec, "%s*.*", szPath);
   pfoHead = DirFindAll (szSpec, FILE_DIRECTORY);
   for (pfo = pfoHead; pfo; pfo = pfo->next)
      {
      sprintf (szSpec, "%s%s\\%s", szPath, pfo->szName, szMatchName);
      DoFileList (szSpec, TRUE, pusFiles, pulXlates, pulLines);
      }
   DirFindAllCleanup (pfoHead);
   }


/***************************************************************************/
/*                                                                         */
/* Info fns and main                                                       */
/*                                                                         */
/***************************************************************************/


void ListXlates (void)
   {
   UINT   i, j, uSize = 0;
   PXLATE pxlate;

   if (!pXLATEHEAD)
      Error ("No translations loaded");

   for (pxlate = pXLATEHEAD; pxlate; pxlate = pxlate->next)
      uSize = max (uSize, strlen (pxlate->pszFrom));

   for (i=0, pxlate = pXLATEHEAD; pxlate; pxlate = pxlate->next, i++)
      {
      j = printf ("%3.3d> \"%s\"", i, pxlate->pszFrom);
      for (; j < uSize + 8; j++)
         putchar (' ');
      printf ("\"%s\"\n", pxlate->pszTo);
      }
   exit (0);
   }


void Usage2 ()
   {
   printf ("GREASE   Text String Replacement Utility  v1.0    %s  %s\n", __TIME__, __DATE__);
   printf (" \n");
   printf (" TRANSLATION FILE FORMAT:\n");
   printf (" ------------------------\n");
   printf (" The translation file is a file containing a list of translations, one per \n");
   printf (" line.  A translation is 2 strings, a search string and a replace string.\n");
   printf (" The file may be in one of 3 formats:\n");
   printf (" If the translation file ends in .CSV it is assumed to be a CSV file\n");
   printf (" If the translation file ends in .TAB it is assumed to be a TAB delimited file.\n");
   printf (" Otherwise the file is expected to be in the form:\n");
   printf (" <delim char><Search String><delim char><Replace String><delim char>\n");
   printf (" where the delim char may be almost any char.\n");
   printf (" \n");
   printf (" Search String may be a regular expression which supports the following:\n");
   printf ("   ? ...... Any 1 char              [] ..... range\n");
   printf ("   * ...... Any chars               {} ..... group (precidence)\n");
   printf ("   @ ...... 0 or more of prev       < ...... start of line\n");
   printf ("   + ...... 1 or more of prev       > ...... end of line\n");
   printf ("   \\char .. any char\n");
   printf (" \n");
   printf (" You may use \\0 thru \\9 in the replacement string to substitute the nth group\n");
   printf (" of source text (ala Brief regular expressions)\n");
   exit (0);
   }


void Usage ()
   {
   printf ("GREASE       Text String Replacement Utility  v3.0       %s  %s\n", __TIME__, __DATE__);
   printf ("\n");
   printf ("USAGE: GREASE [options]  filespec [filespec ...]\n");
   printf ("\n");
   printf ("WHERE: filespec     is the file[s] to process. Wildcards OK.\n");
   printf ("       [options] are 0 or more of:\n");
   printf ("          /h /? ........... This help\n");
   printf ("          /helpxlate ...... Help on xlatefile format and regex.\n");
   printf ("          /R ecurse ........ Recurse Subdirectories\n");
   printf ("          /N oreg .......... Do not use regular expressions\n");
   printf ("          /P artial ........ Replace even partial strings (non regex)\n");
   printf ("          /C ase ........... Case INSENSITIVE\n");
   printf ("          /Q uery .......... Query files before replacing\n");
   printf ("          /L ist ........... List Translations loaded only\n");
   printf ("          /X=Xlatefile .... Translation File (see -helpxlate)\n");
   printf ("          /T=xlatestring .. Translation string\n");
   printf ("          /S wap ........... Swap Src/Dest strings\n");
   printf ("          /Z .............. Test mode, don't really alter files\n");
   printf ("\n");
   printf ("  Either -x or -t option must be used or matching files will just be listed.\n");
   printf ("\n");
   printf ("EXAMPLE: Grease -r -t \".Bilbo Baggins.Frodo Baggins.\" -n Hobb*.doc Hobb*.txt\n");
   exit (0);
   }



cdecl main (int argc, char *argv[])
   {
   UINT  i, uFiles  = 0;
   ULONG ulXlates= 0, ulLines = 0;
   BOOL  bRecurse, bListXlates;

   if (ArgBuildBlk ("?- *^Helpxlate ^r- ^p- ^n-"
                    " ^x% ^t% ^l- ^q- ^s- ^z- ^c- ^o-"))
      Error ("%s", ArgGetErr ());

   if (ArgFillBlk (argv))
      Error ("%s", ArgGetErr ());
   if (ArgIs ("Helpxlate"))
      Usage2 ();
   if (ArgIs ("?") || ArgIs ("Help") || !ArgIs (NULL) && !ArgIs ("l"))
      Usage ();

   bLISTONLY   = !ArgIs("x") && !ArgIs("t");
   bQUERY      =  ArgIs("q");
   bTEST       =  ArgIs("z");
   bREGEX      = !ArgIs("n");
   bSWAP       =  ArgIs("s");
   bCASE       = !ArgIs("c");
   bPARTIALS   =  ArgIs("p");
   bRecurse    =  ArgIs("r");
   bListXlates =  ArgIs("l");
   bOVERLAP    =  ArgIs("o");

   RegCaseSensitive (bCASE);

   for (i=0; i < ArgIs("x"); i++)
      ReadXlateFile (ArgGet("x", i));
   for (i=0; i < ArgIs("t"); i++)
      AddXlate (ArgGet("t", i), NULL);

   if (bListXlates)
      ListXlates ();
   if (!ArgIs(NULL))
      Error ("No File Name Given", "");

   for (i = 0; i < ArgIs(NULL); i++)
      DoFileList (ArgGet(NULL, i), bRecurse, &uFiles, &ulXlates, &ulLines);

   if (uFiles > 1)
      {
      printf ("            -------------------------------------------------------------------\n");
      printf ("            %3.3d Files                                 %5.5ld Xlates, %5.5ld Lines\n",
             uFiles, ulXlates, ulLines);
      }
   return (0);
   }
