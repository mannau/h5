#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP h5_CloseAttribute(SEXP);
extern SEXP h5_CloseDataset(SEXP);
extern SEXP h5_CloseDataspace(SEXP);
extern SEXP h5_CloseFile(SEXP);
extern SEXP h5_CloseGroup(SEXP);
extern SEXP h5_CreateAttribute_DataSet(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP h5_CreateAttribute_Group(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP h5_CreateAttribute_H5File(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP h5_CreateDataset(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP h5_CreateGroup(SEXP, SEXP);
extern SEXP h5_ExistsGroup(SEXP, SEXP);
extern SEXP h5_ExtendDataset(SEXP, SEXP);
extern SEXP h5_FlushFile(SEXP);
extern SEXP h5_GetAttributeDimensions(SEXP);
extern SEXP h5_GetAttributeNames_CommonFG(SEXP);
extern SEXP h5_GetAttributeNames_DataSet(SEXP);
extern SEXP h5_GetAttributeType(SEXP);
extern SEXP h5_GetDataSetChunksize(SEXP);
extern SEXP h5_GetDataSetCompression(SEXP);
extern SEXP h5_GetDataSetDimensions(SEXP);
extern SEXP h5_GetDataSetMaxDimensions(SEXP);
extern SEXP h5_GetDataSetNames(SEXP, SEXP, SEXP);
extern SEXP h5_GetDataSetType(SEXP);
extern SEXP h5_GetDataspace(SEXP);
extern SEXP h5_GetGroupNames(SEXP, SEXP, SEXP);
extern SEXP h5_GetSoftLinks(SEXP, SEXP);
extern SEXP h5_IsHDF5File(SEXP);
extern SEXP h5_OpenAttribute_DataSet(SEXP, SEXP);
extern SEXP h5_OpenAttribute_Group(SEXP, SEXP);
extern SEXP h5_OpenAttribute_H5File(SEXP, SEXP);
extern SEXP h5_OpenDataset(SEXP, SEXP);
extern SEXP h5_OpenFile(SEXP, SEXP);
extern SEXP h5_OpenGroup(SEXP, SEXP);
extern SEXP h5_ReadAttribute(SEXP, SEXP);
extern SEXP h5_ReadDataset(SEXP, SEXP, SEXP);
extern SEXP h5_SelectAll(SEXP);
extern SEXP h5_SelectElem(SEXP, SEXP);
extern SEXP h5_SelectHyperslab(SEXP, SEXP, SEXP, SEXP);
extern SEXP h5_Unlink(SEXP, SEXP);
extern SEXP h5_WriteAttribute(SEXP, SEXP, SEXP, SEXP);
extern SEXP h5_WriteDataset(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"h5_CloseAttribute",             (DL_FUNC) &h5_CloseAttribute,             1},
  {"h5_CloseDataset",               (DL_FUNC) &h5_CloseDataset,               1},
  {"h5_CloseDataspace",             (DL_FUNC) &h5_CloseDataspace,             1},
  {"h5_CloseFile",                  (DL_FUNC) &h5_CloseFile,                  1},
  {"h5_CloseGroup",                 (DL_FUNC) &h5_CloseGroup,                 1},
  {"h5_CreateAttribute_DataSet",    (DL_FUNC) &h5_CreateAttribute_DataSet,    5},
  {"h5_CreateAttribute_Group",      (DL_FUNC) &h5_CreateAttribute_Group,      5},
  {"h5_CreateAttribute_H5File",     (DL_FUNC) &h5_CreateAttribute_H5File,     5},
  {"h5_CreateDataset",              (DL_FUNC) &h5_CreateDataset,              8},
  {"h5_CreateGroup",                (DL_FUNC) &h5_CreateGroup,                2},
  {"h5_ExistsGroup",                (DL_FUNC) &h5_ExistsGroup,                2},
  {"h5_ExtendDataset",              (DL_FUNC) &h5_ExtendDataset,              2},
  {"h5_FlushFile",                  (DL_FUNC) &h5_FlushFile,                  1},
  {"h5_GetAttributeDimensions",     (DL_FUNC) &h5_GetAttributeDimensions,     1},
  {"h5_GetAttributeNames_CommonFG", (DL_FUNC) &h5_GetAttributeNames_CommonFG, 1},
  {"h5_GetAttributeNames_DataSet",  (DL_FUNC) &h5_GetAttributeNames_DataSet,  1},
  {"h5_GetAttributeType",           (DL_FUNC) &h5_GetAttributeType,           1},
  {"h5_GetDataSetChunksize",        (DL_FUNC) &h5_GetDataSetChunksize,        1},
  {"h5_GetDataSetCompression",      (DL_FUNC) &h5_GetDataSetCompression,      1},
  {"h5_GetDataSetDimensions",       (DL_FUNC) &h5_GetDataSetDimensions,       1},
  {"h5_GetDataSetMaxDimensions",    (DL_FUNC) &h5_GetDataSetMaxDimensions,    1},
  {"h5_GetDataSetNames",            (DL_FUNC) &h5_GetDataSetNames,            3},
  {"h5_GetDataSetType",             (DL_FUNC) &h5_GetDataSetType,             1},
  {"h5_GetDataspace",               (DL_FUNC) &h5_GetDataspace,               1},
  {"h5_GetGroupNames",              (DL_FUNC) &h5_GetGroupNames,              3},
  {"h5_GetSoftLinks",               (DL_FUNC) &h5_GetSoftLinks,               2},
  {"h5_IsHDF5File",                 (DL_FUNC) &h5_IsHDF5File,                 1},
  {"h5_OpenAttribute_DataSet",      (DL_FUNC) &h5_OpenAttribute_DataSet,      2},
  {"h5_OpenAttribute_Group",        (DL_FUNC) &h5_OpenAttribute_Group,        2},
  {"h5_OpenAttribute_H5File",       (DL_FUNC) &h5_OpenAttribute_H5File,       2},
  {"h5_OpenDataset",                (DL_FUNC) &h5_OpenDataset,                2},
  {"h5_OpenFile",                   (DL_FUNC) &h5_OpenFile,                   2},
  {"h5_OpenGroup",                  (DL_FUNC) &h5_OpenGroup,                  2},
  {"h5_ReadAttribute",              (DL_FUNC) &h5_ReadAttribute,              2},
  {"h5_ReadDataset",                (DL_FUNC) &h5_ReadDataset,                3},
  {"h5_SelectAll",                  (DL_FUNC) &h5_SelectAll,                  1},
  {"h5_SelectElem",                 (DL_FUNC) &h5_SelectElem,                 2},
  {"h5_SelectHyperslab",            (DL_FUNC) &h5_SelectHyperslab,            4},
  {"h5_Unlink",                     (DL_FUNC) &h5_Unlink,                     2},
  {"h5_WriteAttribute",             (DL_FUNC) &h5_WriteAttribute,             4},
  {"h5_WriteDataset",               (DL_FUNC) &h5_WriteDataset,               5},
  {NULL, NULL, 0}
};

void R_init_h5(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}