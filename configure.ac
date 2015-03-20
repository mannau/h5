AC_INIT([h5], 0.9, [mario.annau@gmail.com])

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

CC=`"${R_HOME}/bin/R" CMD config CC`
CPP=`"${R_HOME}/bin/R" CMD config CPP`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

dnl test for new debian version, set LIBS and CPPFLAGS accordingly
dnl TODO: should make h5c++ the default approach for newer versions
dnl LIBS=`h5c++ -show | grep -o "[^[:space:]]\+\.a" | tr "\n" " "`

[
LIBS=`h5c++ -show | grep -o "\-L[^[:space:]]\+" | tr "\n" " "` 
]

LIBS="${LIBS} -L. -lhdf5_cpp -lhdf5 -lz -lm"
if test -d /usr/include/hdf5/serial; then
    CPPFLAGS="${CPPFLAGS} -I/usr/include/hdf5/serial"
fi

dnl Check the headers can be found
dnl AC_CHECK_HEADERS(hdf5.h)
dnl if test "${ac_cv_header_hdf5_h}" = no || 
dnl     test "${ac_cv_header_h5cpp_h}" = no; then
dnl     AC_MSG_ERROR("HDF5 headers hdf5.h not found")
dnl fi

dnl substitute RODBC_CPPFLAGS and LIBS
AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
dnl and do subsitution in the src/Makevars.in 
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
