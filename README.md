# uzura2_fpm

uzura2 for fpm (Fortran package manager) ver 0.4

mp2 enocder written in Fortran 2008 



Fortran Package Manager is required.
https://github.com/fortran-lang/fpm



$cd uzura2_fpm

$fpm build --profile release

$fpm run

```
 Usage : uzura2 -option file_name
       : file_name.wav -> file_name.mp2
 Option: -b 1..14  bitrate (default 12: 256kbps)
         -crc      CRC16 error protection on
         -c        copyright flag on
         -o        original  flag on
```

$fpm run -- -b 10 file_name