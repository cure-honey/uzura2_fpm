    module layer2_m
        use :: kind_m
        implicit none
        private
        public :: subband_normalization, bit_allocation, quantization, &
                  initialize_table, select_table, table, layer2_table, nscfsi
        
        type :: layer2_table
            integer :: nsteps(0:17), nsample(0:17), nlength(0:17)
            integer :: nband
            integer :: nbal(32)
            integer :: nbit(32, 0:15)
        end type layer2_table
        
        type(layer2_table) :: table
        
        ! ISO Table C.4 -- Layer II  scalefactor transmission patterns
        integer, parameter :: itransmission_pattern(5, 5) = &
            reshape( [123, 122, 122, 133, 123, &
                      113, 111, 111, 444, 113, &
                      111, 111, 111, 333, 113, &
                      222, 222, 222, 333, 123, &
                      123, 122, 122, 133, 123  ], [5, 5] )
        ! ISO Table C.4 -- Layer II  scalefactor transmission patterns  
        integer, parameter :: iscale_factor_select_information(5, 5) = &
            reshape( [0, 3, 3, 3, 0, &
                      1, 2, 2, 2, 1, &
                      2, 2, 2, 2, 1, &
                      2, 2, 2, 2, 0, &
                      0, 3, 3, 3, 0  ], [5, 5] )
        
        ! ISO Table C.5 -- Layer II  Signal-to-Noise Ratios
        real(kd), parameter :: snr(0:*) =[ 0.00_kd,  7.00_kd, 11.00_kd, 16.00_kd, 20.84_kd, 25.28_kd, 31.59_kd, 37.75_kd, &
                                43.84_kd, 49.89_kd, 55.93_kd, 61.96_kd, 67.98_kd, 74.01_kd, 80.03_kd, 86.05_kd, 92.01_kd ]

        integer, parameter :: nscfsi(0:3) = [3, 2, 1, 2]

        real(kd) :: scale_factor(0:63), a(17), b(17)
    
    contains
    
        ! ISO Table B.2 -- Layer II  bit allocation tables        
        subroutine select_table(isample_rate, ibit_rate, nchannel)
            integer, intent(in) :: isample_rate, ibit_rate, nchannel
            integer :: itab
            integer, parameter :: itable(0:14, 3, 2) = &
               reshape(  [ & ! mono
                2, 3, 3, 1, 1, 1, 2, 2, 2, 2, 2, -1, -1, -1, -1, &
                1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, &
                2, 4, 4, 1, 1, 1, 2, 2, 2, 2, 2, -1, -1, -1, -1, &
                ! stereo
                2, 3, 3, 3, 3, 3, 3, 1, 1, 1, 2,  2,  2,  2,  2, &
                1, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1,  1,  1,  1,  1, &
                2, 4, 4, 4, 4, 4, 4, 1, 1, 1, 2,  2,  2,  2,  2  ], [15, 3, 2] )
            itab = itable( ibit_rate, isample_rate + 1, nchannel )
            if (itab < 0) then 
                stop 'unexpected bit_rate: select_table'
            else
                call initialize_table(itab)
            end if
        end subroutine select_table

    
        ! ISO Table B.2 -- Layer II  bit allocation tables    
        subroutine initialize_table(itable)
            integer, intent(in) :: itable
            table%nsteps(0:17)  = [0, 3, 5, 7, 9, 15, 31, 63, 127, 255, 511, 1023, 2047, 4095, 8191, 16383, 32767, 65535]
            table%nsample(0:17) = [1, 3, 3, 1, 3,  1,  1,  1,   1,   1,   1,    1,    1,    1,    1,     1,     1,     1]
            table%nlength(0:17) = [0, 5, 7, 3,10,  4,  5,  6,   7,   8,   9,   10,   11,   12,   13,    14,    15,    16]
            select case (itable)
            case (1)
                table%nband = 27
                table%nbal = [4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
                              4, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
                              3, 3, 3, 2, 2, 2, 2, 0, 0, 0, 0, 0]
                table%nbit( 1, :) = [0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17]
                table%nbit( 2, :) = [0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17]
                table%nbit( 3, :) = [0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17]
                table%nbit( 4, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 5, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 6, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 7, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 8, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 9, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit(10, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit(11, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit(12, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(13, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(14, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(15, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(16, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(17, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(18, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(19, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(20, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(21, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(22, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(23, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(24, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(25, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(26, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(27, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(28, :) = [0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(29, :) = [0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(30, :) = [0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(31, :) = [0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(32, :) = [0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
            case (2)
                table%nband = 30
                table%nbal = [4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
                              4, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
                              3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 0, 0]
                table%nbit( 1, :) = [0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17]
                table%nbit( 2, :) = [0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17]
                table%nbit( 3, :) = [0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17]
                table%nbit( 4, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 5, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 6, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 7, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 8, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit( 9, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit(10, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit(11, :) = [0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17]
                table%nbit(12, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(13, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(14, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(15, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(16, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(17, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(18, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(19, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(20, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(21, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(22, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(23, :) = [0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(24, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(25, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(26, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(27, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(28, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(29, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(30, :) = [0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(31, :) = [0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(32, :) = [0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0]
            case (3)
                table%nband = 8
                table%nbal = [4, 4, 3, 3, 3, 3, 3, 3, 0, 0, &
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                table%nbit( 1, :) = [0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
                table%nbit( 2, :) = [0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
                table%nbit( 3, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 4, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 5, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 6, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 7, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 8, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 9, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(10, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(11, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(12, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(13, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(14, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(15, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(16, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(17, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(18, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(19, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(20, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(21, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(22, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(23, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(24, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(25, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(26, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(27, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(28, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(29, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(30, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(31, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(32, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
            case (4)
                table%nband = 12
                table%nbal = [4, 4, 3, 3, 3, 3, 3, 3, 3, 3, &
                              3, 3, 0, 0, 0, 0, 0, 0, 0, 0, &
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                table%nbit( 1, :) = [0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
                table%nbit( 2, :) = [0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
                table%nbit( 3, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 4, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 5, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 6, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 7, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 8, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit( 9, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(10, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(11, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(12, :) = [0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(13, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(14, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(15, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(16, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(17, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(18, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(19, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(20, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(21, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(22, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(23, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(24, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(25, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(26, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(27, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(28, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(29, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(30, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(31, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
                table%nbit(32, :) = [0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0]
            case default
                stop 'input error in initalize_table'
            end select
        end subroutine initialize_table

        
        subroutine initialize_scalefactor()
            integer :: i
            forall(i = 0:2**6 - 1) scale_factor(i) = 1.0e-14_kd * anint( 1.0e14_kd * 2.0_kd * 2.0_kd**( -real(i, kd) / 3.0_kd ) )
        end subroutine initialize_scalefactor

        
        subroutine subband_normalization(subband, iscale_factor, iscfsi)
            real (kd), intent(in out) :: subband(:, :, :)
            integer, intent(out) :: iscale_factor(:, :, :)
            integer, intent(out) :: iscfsi(:, :)
            integer :: i, i0, i1, iband, ichannel
            logical, save :: qfirst = .true.
            if (qfirst) then
                qfirst = .false.
                call initialize_scalefactor()
            end if
            call get_scale_factor(subband, iscale_factor)
            call scale_factor_select_information(iscale_factor, iscfsi)
            do i = 1, 3
                i0 = 12 * (i - 1) + 1
                i1 = 12 *  i
                do iband = 1, table%nband
                    do ichannel = 1, size(subband, 3)
                        subband(iband, i0:i1, ichannel) = & 
                            subband(iband, i0:i1, ichannel) / scale_factor(iscale_factor(iband, i, ichannel))
                    end do
                end do
            end do
        end subroutine subband_normalization

        
        subroutine get_scale_factor(subband, iscale_factor)
            real (kd), intent(in) :: subband(:, :, :)
            integer, intent(out) :: iscale_factor(:, :, :)
            real (kd) :: subband_max(1)
            integer :: i, i0, i1, ichannel, iband, k
            do i = 1, 3
                i0 = 12 * (i - 1) + 1
                i1 = 12 *  i
                do ichannel = 1, size(subband, 3)
                    do iband = 1, 32
                        subband_max = maxval( abs( subband(iband, i0:i1, ichannel) ) )
                        do k = 62, 0, -1
                            if ( subband_max(1) < scale_factor(k) ) then
                                iscale_factor(iband, i, ichannel) = k
                                exit
                            end if
                        end do
                    end do
                end do
            end do
        end subroutine get_scale_factor

        
        subroutine scale_factor_select_information(iscale_factor, iscfsi)
            integer, intent(in out) :: iscale_factor(:, :, :)
            integer, intent(   out) :: iscfsi(:, :)
            integer :: iband, ichannel, mscf1, mscf2, iclass1, iclass2, ipattern
            do ichannel = 1, size(iscfsi, 2)
                do iband = 1, 32
                    mscf1 = iscale_factor(iband, 1, ichannel) - iscale_factor(iband, 2, ichannel)
                    mscf2 = iscale_factor(iband, 2, ichannel) - iscale_factor(iband, 3, ichannel)
                    iclass1 = iget_class(mscf1)
                    iclass2 = iget_class(mscf2)
                    ipattern = itransmission_pattern(iclass2, iclass1)
                    call set_scale(iscale_factor(iband, :, ichannel), ipattern)
                    iscfsi(iband, ichannel) = iscale_factor_select_information(iclass2, iclass1)
                end do
            end do
        end subroutine scale_factor_select_information

        
        function iget_class(m) result(ires)
            integer, intent(in) :: m
            integer :: ires
            select case (m)
            case (:-3)
                ires = 1
            case (-2:-1)
                ires = 2
            case (0)
                ires = 3
            case (1:2)
                ires = 4
            case (3:)
                ires = 5
            end select
        end function iget_class

        
        subroutine set_scale(iscale, ipattern)
            integer, intent(in out) :: iscale(:)
            integer, intent(in    ) :: ipattern
            select case (ipattern)
            case (123)
                continue
            case (122)
                iscale(3) = iscale(2)
            case (133)
                iscale(2) = iscale(3)
            case (113)
                iscale(2) = iscale(1)
            case (111)
                iscale(2) = iscale(1)
                iscale(3) = iscale(1)
            case (222)
                iscale(1) = iscale(2)
                iscale(3) = iscale(2)
            case (333)
                iscale(1) = iscale(3)
                iscale(2) = iscale(3)
            case (444)
                iscale(1) = min( iscale(1), iscale(2), iscale(3) )
                iscale(2) = iscale(1)
                iscale(3) = iscale(1)
            case default
                stop 'undefined transmission pattern: set_scale'
            end select
        end subroutine set_scale

        
        subroutine bit_allocation(smr, iscfsi, ialloc_bits, itot_bits, max_bits)
            real (kd), intent(in    ) :: smr(:, :)
            integer        , intent(in    ) :: max_bits, iscfsi(:, :)
            integer        , intent(   out) :: ialloc_bits(:, :)
            integer        , intent(in out) :: itot_bits
            integer :: ireq_bits(size(smr, 1), size(smr, 2))
            integer :: iband, max_pos(2), k, n0, n1
            real(kd) :: amnr(size(smr, 1), size(smr, 2))
            
            ialloc_bits = 0
            amnr = smr - snr(0)
            ireq_bits = 999999 ! large number
            do iband = 1, table%nband
                ireq_bits(iband, :) = 2 & ! scfsi
                                    + 6 * nscfsi(iscfsi(iband, :)) & ! scale_factor
                                    + 3 / table%nsample(1) * table%nlength(1) * 12 ! data
            end do
            do
                max_pos = maxloc(amnr, mask = ireq_bits < max_bits - itot_bits)
                if ( max_pos(1) <= 0 .or. max_pos(2) <= 0 ) exit
                if ( amnr( max_pos(1), max_pos(2) ) < -200.0 ) exit
                itot_bits = itot_bits + ireq_bits( max_pos(1), max_pos(2) )
                k = ialloc_bits( max_pos(1), max_pos(2) )
                if (k > 14) exit
 
                if ( k == 2**table%nbal(max_pos(1)) - 2 ) then ! true iso
                    ireq_bits( max_pos(1), max_pos(2) ) = 999999  ! maximum allowed bits
                else if ( table%nbal(max_pos(1)) /= 4 .and. k == 2**table%nbal(max_pos(1)) - 3 ) then ! don't use maximum bit_alloc in higher bands non-iso
                    ireq_bits( max_pos(1), max_pos(2) ) = 999999
                else
                    n0 = table%nbit(max_pos(1) , k    )
                    n1 = table%nbit(max_pos(1) , k + 1)
                    ireq_bits( max_pos(1), max_pos(2) ) = 3 / table%nsample(n1) * table%nlength(n1) * 12 &
                                                        - 3 / table%nsample(n0) * table%nlength(n0) * 12
                end if
                ialloc_bits( max_pos(1), max_pos(2) ) = k + 1
                amnr       ( max_pos(1), max_pos(2) ) = smr( max_pos(1), max_pos(2) ) - snr(k + 1)
            end do
        end subroutine bit_allocation
        
        
        ! ISO Table C.6 -- Layer II  quantization coefficients
        subroutine initialize_quantization()
            integer :: i
            b(1) = -1.0d-9 * anint( 1.0d9 / 2.0_kd**2.0_kd )
            a(1) =  1.0_kd + b(1)
            b(2) = -1.0d-9 * anint( 3.0d9 / 8.0_kd )
            a(2) =  1.0_kd + b(2)
            b(3) = -1.0d-9 * anint( 1.0d9 / 2.0_kd**3.0_kd )
            a(3) =  1.0_kd + b(3)
            b(4) = -1.0d-9 * anint( 7.0d9 / 16.0_kd )
            a(4) =  1.0_kd + b(4)
            do i = 5, 17
                b(i) = -1.0d-9 * anint(1.0d9 / 2.0_kd**real(i - 1, kd))
                a(i) =  1.0_kd + b(i)
            end do
        end subroutine initialize_quantization

        
        subroutine quantization(ialloc_bits, subband, isubband)
            integer, intent(in) :: ialloc_bits(:, :)
            real (kd), intent(in) ::  subband(:, :, :)
            integer, intent(out) :: isubband(:, :, :)
            integer :: ichannel, iband, msb, k, i, j, j0, j1
            integer, parameter :: imsb(0:17) = [0, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]
            logical, save :: qfirst = .true.
            real(kd) :: s(12)
            if (qfirst) then
                qfirst = .false.
                call initialize_quantization()
            end if
            do ichannel = 1, size(subband, 3)
                do iband = 1, table%nband
                    k = table%nbit(iband, ialloc_bits(iband, ichannel))
                    msb = 2**imsb(k)
                    if (k /= 0) then
                        do j = 1, 3
                            j0 = 12 * (j - 1) + 1
                            j1 = 12 * j
                            s = a(k) * subband(iband, j0:j1, ichannel) + b(k)
                            do i = 1, 12
                                if (s(i) >= 0.0) then
                                    isubband(iband, j0 - 1 + i, ichannel) = int( s(i) * real(msb, kd) ) + msb
                                else
                                    isubband(iband, j0 - 1 + i, ichannel) = int( (s(i) + 1.0_kd) * real(msb, kd) )
                                end if
                            end do
                        end do
                    else
                        isubband(iband, :, ichannel) = 0
                    end if
                end do
            end do
        end subroutine quantization

    end module layer2_m