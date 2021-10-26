# uzura2_fpm

mp2 enocder.

$cd uzura2_fpm

$fpm build --profile release

$fpm run

```
 Usage : uzura -option file_name
       : file_name.wav -> file_name.mp2
 Option: -b 1..14  bitrate (default 12: 256kbps)
         -crc      CRC16 error protection on
         -c        copyright flag on
         -o        original  flag on
```

$uzura2 -b 10 fine_name