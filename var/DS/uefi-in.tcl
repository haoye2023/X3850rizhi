# 07 Revision:35 !!Always denote Revision as 2 digits. DO NOT add anything above this line !!
# This is IMPORTANT for UEHandling to check tcl script version with IMM
# uefi-tcl.in
# UEFI input tcl script of Brickland for the IMM
#
# Copyright (c) 2011 - 2021  Lenovo Corporation
# Last Update: 01/14/2021
#
# This file is run under the IMM 'safeshell' script. It will collect information
# from the IMM and place it in a file under datastore, for later reading by UEFI
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Global variables define
#--------------------------------------------------------------------------------
set PRE \$::
#
# Global Definition
#
# below global definitions can be overrided by UEFI code
# check final TCL in var\DS for final global configuration
#
set TARGET_CPU      1
set MAX_CPU         0
set MAX_IIO         0
set MAX_JC          0
set MAX_CORE        18
set MCA_BANK_NUM    32
set MCA_BANK_MEM_MINNUM 7
set MCA_BANK_MEM_MAXNUM 16
set HA_PER_SOCKET 	2
set CHANNEL_PER_IMC	4
set TOTAL_CHANNEL   [expr $::HA_PER_SOCKET * $::CHANNEL_PER_IMC]
set MCA_BASE		0x400
set MAX_QPI_PORT	3
set QPI_MCA_BASE	0x5E0
set EVENT_PRIORITY { mem mem_ha mem_mb mem_ps qpi iio cpu}
set VGPIO {}
set VGPIO_LOG_BUFFER {}
set REG_STORE REGISTERS
set QPIPOISON_ENTRY 0
set EX_SUPPORT      1
set MB_NUMBER       4
# SAD_DRAM: DRAM_RULE number
# SAD_MMIO: MMIO_RULE number
# TAD: TAD[0:11] TADWAYNESS_[0:11] TADCHNILVOFFSET_[0:11]
# RIR:
array set DECODE_ENTRIES {SAD_MMIO 16 SAD_DRAM 20 TAD 12 RIR 5}
set chXorList [list 12 14 16 18 20 22 24 26]

#define TCL SCRIPT GUID as 8B47B23E-99A6-4795-A98C-2F56C3812C7D
set SCRIPT_GUID 8B47B23E99A64795A98C2F56C3812C7D
#define IBM Auxlog Entry GUID as C2602024-36F9-418D-80C1-54EC988B9917
#note that the first three segments of guid are stored in little-endian format.
set IBM_AUXLOG_ENTRY_GUID 242060C2F9368D4180C154EC988B9917

#=========== Debug Level Set =============
set D_ERROR         0
set D_WARNING       1
set D_INFO          2
set D_VERBOSE       3
set D_VERBOSE_MORE  4
#=========================================
set DEBUG_MSG_LEVEL $D_VERBOSE
set DEBUG_FUNCS [list scanRange]
set DEBUG_SHOW_CALLER 1

proc debug {message {level D_INFO}} {
    set level [subst $::PRE$level]
    set caller [lindex [split [info level [expr [info level] - 1]]] 0]
    
    if {$level > $::DEBUG_MSG_LEVEL && [lsearch -exact $::DEBUG_FUNCS $caller] == -1} {
        return
    }
    
    if {$::DEBUG_SHOW_CALLER} {
        set message "$caller: $message"
    }
    
    puts $message
}

# data        = binary data
# start_addr  = starting address of the first output column
#               (affects address column display only)
# width       = bytes to display per line
proc hexdump {data {start_addr 0} {width 16}} {
    set out ""
    if { [ catch {
        # Convert the data to hex and to characters.
        binary scan $data H*@0a* hex ascii
        # Replace non-printing characters in the data.
        regsub -all -- {[^[:graph:] ]} $ascii {.} ascii
        set nbytes [string length $ascii]
        for {set pos 0} {$pos < $nbytes} {incr pos $width} {
            set addr [expr $pos + $start_addr]
            set s_hex [string range $hex [expr $pos * 2] [expr ($pos + $width)*2 - 1]]
            set s_ascii [string range $ascii $pos [expr $pos + $width - 1]]

            # Convert the hex to pairs of hex digits
            regsub -all -- {..} $s_hex {& } fmt_hex

            # Put the hex and Latin-1 data to the channel
            append out [format "%06x %-[expr $width*3]s %-8s\n" $addr $fmt_hex $s_ascii]
        }
    } err ] } {
        return -code error $err
    }
    return $out
}

set AuxLogHeader.Prototype {
    {CodeType:i     0x90000002}
    Value:i 
    Instance:i
    {CallerId $::SCRIPT_GUID}
    %DataHeader%
}

set DataHeader.Prototype {
    {HeaderSize:s   0x0014}
    {Size:s     0x0000}
    {Type $::IBM_AUXLOG_ENTRY_GUID}
}

set RawData.Prototype {
    {HeaderSize:s 0x0008}
    {Size:s     0x0000}
    {Signature:A WAR_}
    Data:A  
}

set AsciiData.Prototype {
    {HeaderSize:s 0x0008}
    {Size:s         0x0000}
    {Signature:A CSA_}
    Data:A
}

set AsciiLog.Prototype [concat ${AuxLogHeader.Prototype} %AsciiData%]
set FullLog.Prototype [concat ${AuxLogHeader.Prototype} %RawData% %AsciiData%]

set IioErrorReg.Prototype {
    {PciConfigAddress   00000000}
    {RegGSYSST          {[getCSRLocal 0x05 0x02 0x1CC]}}
    {RegGFERRST         {[getCSRLocal 0x05 0x02 0x1C4]}}
    {RegGNERRST         {[getCSRLocal 0x05 0x02 0x1C0]}}
    {RegGERRCTL         {[getCSRLocal 0x05 0x02 0x1C8]}}
    {RegGFFERRST        {[getCSRLocal 0x05 0x02 0x1DC]}}
    {RegGFNERRST        {[getCSRLocal 0x05 0x02 0x1E8]}}
    {RegGNFERRST        {[getCSRLocal 0x05 0x02 0x1EC]}}
    {RegGNNERRST        {[getCSRLocal 0x05 0x02 0x1F8]}}
    {RegXPGLBERRSTS     {[getCSRLocal $dev $func 0x230]}}
    {RegXPGLBERRPTR     {[getCSRLocal $dev $func 0x232]}}
    {RegXPUNCERRSTS     {[getCSRLocal $dev $func 0x208]}}
    {RegXPUNCERRPTR     {[getCSRLocal $dev $func 0x214]}}
    {RegUNCERRSTS       {[getCSRLocal $dev $func 0x14C]}}
    {RegIIOERRST        {[getCSRLocal 0x05 0x02 0x300]}}
    {RegIIOFFERRST      {[getCSRLocal 0x05 0x02 0x308]}}
    {RegIIOFFERRHD      {[getCSRLocal 0x05 0x02 0x30C]}}
    {RegIIOFNERRST      {[getCSRLocal 0x05 0x02 0x31C]}}
    {RegIIONFERRST      {[getCSRLocal 0x05 0x02 0x320]}}
    {RegIIONFERRHD      {[getCSRLocal 0x05 0x02 0x324]}}
    {RegIIONNERRST      {[getCSRLocal 0x05 0x02 0x334]}}
    {RegIIOERRCNTSEL    {[getCSRLocal 0x05 0x02 0x33C]}}
    {RegIIOERRCNT       {[getCSRLocal 0x05 0x02 0x340]}}
    {RegTHRERRST        FFFFFFFF}
    {RegTHRERRCNTSEL    FFFFFFFF}
    {RegTHRERRCNT       FFFFFFFF}
    {RegLER_CTRLSTS     {[getCSRLocal $dev $func 0x288]}}
    {RegRPERRSTS        {[getCSRLocal $dev $func 0x178]}}
    {RegERRSID          {[getCSRLocal $dev $func 0x17C]}}
    {RegVTSTS           FFFFFFFF}
    {RegVTUNCERRSTS     {[getCSRLocal 0x05 0x00 0x1A8]}}
    {RegVTUNCERRPTR     {[getCSRLocal 0x05 0x00 0x1B4]}}
    {RegQPI0ERRST       {[getCSRLocal 0x08 0x00 0x078]}}
    {RegQPI1ERRST       {[getCSRLocal 0x08 0x00 0x07C]}}
    {RegQPIP0ERRST      {[getCSRLocal 0x09 0x00 0x078]}}
    {RegQPIP1ERRST      {[getCSRLocal 0x09 0x00 0x07C]}}
} 

proc getCSRLocal {dev func offset} {
    set addr [mapCSR 0 $dev $func $offset]
    
    debug "getCSRLocal ::REGISTERS(CSR:$addr-$::TARGET_CPU)"
    if {[info exist ::REGISTERS(CSR:$addr-$::TARGET_CPU)]} then {
        set value $::REGISTERS(CSR:$addr-$::TARGET_CPU)
        return [format %08X $value]
    } else {
        return FFFFFFFF
    }
}

proc setProperty {this path value} {
    uplevel set $this.$path $value
}

proc instantiate {prototype name {initValue {}}} {
    set type [subst \{::$prototype\}]
    set type [uplevel set $type]
    set ::Template($name) {}
    
    foreach {var} $type {
        if {[string match %*% $var]} then { #recurrsivly call createFromPortotype for nested prototype
            set varname [string trim $var "%"]
            set subtype [subst \{$varname.Prototype\}]
            uplevel instantiate $subtype $name.$varname
        } else { #declare variable 
            set default \[list\]
            if { [llength $var] > 1} then {
                set default [uplevel subst [lindex $var 1]]
                set var [lindex $var 0]
            }
            set format [lindex [split $var :] 1] 
            set var [lindex [split $var :] 0] 
            
            if {$format==""} then {
                set format H
            }   
            
            uplevel set $name.$var \"$default\" 
            set ::Format($name.$var) $format
            #puts $name.$var
            lappend ::Template([lindex [split $name .] 0]) $name.$var
        }
    }
    
    foreach {field value} $initValue {
        set target [subst \{$name.$field\}]
        set exist [uplevel info exist $target]
        if {$exist < 1} then {
            error "$target does not exist in prototype"
        }
        uplevel set $target \{$value\}
    }
}

proc serialize {instance } {
    set pre \$
    set targets [uplevel info vars $instance*]
    set formatter A
    set data {}
    set template $::Template([lindex [split $instance .] 0])
    
    
    foreach {item} $template {
        if { [lsearch $targets $item] > -1} then {
            set item [subst \{$item\}]
            set value [uplevel subst $pre$item ]
            set formatter $::Format([string trim $item \{\}])
            append data [binary format $formatter* $value]
        }
    }
    return $data
}
 
proc finalizeLog {log} {
    set logSize 0
    if { [uplevel info exist $log.AsciiData.Data] } then {
        set temp [uplevel serialize $log.AsciiData]
        set size [string length $temp]
        set logSize [expr $logSize + $size]
    }
    
    if { [uplevel info exist $log.RawData.Data] } then {        
        set temp [uplevel serialize $log.RawData]
        set size [string length $temp]
        set logSize [expr $logSize + $size]     
    }

    set logSize [format 0x%04X $logSize]
    
    uplevel setProperty $log DataHeader.Size $logSize   
 }

 proc vgpio_on_primary {node event vgpio extended_data auxdata} {
    set cmd [split $vgpio { }]
    set digits [split [lindex $cmd 0] "."]
    if {$::EX_SUPPORT} {
        # EX always use 1-based implementation in vgpio-err.log for uEFI
        set VgpioNode [expr $node+1]
    } else {
        set VgpioNode [expr $node]  
    }
    set digits [lreplace $digits 2 2 $VgpioNode]
    set mod_cmd [join $digits "."]
    lappend  ::VGPIO_LOG_BUFFER [subst "$mod_cmd [lindex $cmd 1] $extended_data"]
    #debug "VGPIO_LOG_BUFFER = $::VGPIO_LOG_BUFFER"
    
    if {$node == 0} {
        set auxcmd ""
        
        if {[llength $auxdata] > 0} then { #assert vgpio with auxdata
            create_auxdata $auxdata
            set auxcmd "auxdata.bin"
        }   
        do_vgpio [lindex $cmd 0] [lindex $cmd 1] assert $auxcmd
    } else {
        add_log [subst vgpio_out$node] "$event $vgpio"
    }
 }
 
 proc vgpio_on_remote {node event vgpio extended_data auxdata} {
    
    if {!$::RemotePhase} {
        #For first phase, only append vgpio found to temp output buffer
        debug "At remote node phase 1, create temp vgpio $vgpio"
        lappend ::VGPIO_LOG_BUFFER [subst "$event $vgpio $extended_data"]
    } else {
        #debug "At remote node phase 2, searching $vgpio in final list"
        if {[lsearch    $::FINAL_VGPIO $vgpio] != -1 } {
            #debug "going to assert $vgpio"
            set auxcmd ""
            set cmd [split $vgpio { }]
            if {[llength $auxdata] > 0} then { #assert vgpio with auxdata
                create_auxdata $auxdata
                set auxcmd "auxdata.bin"
            }       
            do_vgpio [lindex $cmd 0] [lindex $cmd 1] assert $auxcmd
        }
    }
 }
 
#--------------------------------------------------------------------------------
# fireVGPIO --
#   Iteratively calling do_vgpio procedure to fire vgpio store in $::VGPIO list
#--------------------------------------------------------------------------------
proc fireVGPIO {} {
    # dump all error found
    #debug "VGPIO=$::VGPIO"
    foreach {item} $::VGPIO  {
        foreach {node event vgpio extended_data auxdata} $item {
            debug "node=$node errType=$event vgpio=$vgpio"
            debug "extended_data=$extended_data"
            debug "AuxData:\n[hexdump $auxdata]"
        }
    }
    
    if {$::IsPrimary} {
        set vgpio_action vgpio_on_primary
        set prioritized 1
    } else {
        set vgpio_action vgpio_on_remote
        set prioritized 0
    }
    
    foreach {event} $::EVENT_PRIORITY {
        debug "event = $event"      
        set founds  [lsearch  -index 1 -all -inline $::VGPIO $event]
        #debug "founds = $founds"
        #debug "set founds event $event"
        foreach {item} $founds {
            #debug "item = $item"
            foreach {node event vgpio extended_data auxdata} $item {
                debug "action = $vgpio_action $node $event $vgpio $extended_data"
                $vgpio_action $node $event $vgpio $extended_data $auxdata
            }   
        }   
        #We need a more precised logic
        if {[llength $founds] > 0 && $prioritized} {return}
    }
}

#--------------------------------------------------------------------------------
# pow --
#   Calculate mathmatical power as x ^ y.
#--------------------------------------------------------------------------------
proc pow {x y} {
    set temp 1
    
    for { set i 0 } { $i < $y  } { incr i } { 
        set temp [expr $temp * $x]
    }
    return $temp
}

#--------------------------------------------------------------------------------
# log --
#   Calculate logarithm as log(x)/log(y). Only work when x > y !
#--------------------------------------------------------------------------------
proc log {x y} {
    
    set result 0
    
    if { $x < 1} {
        error "We don't do Log(x) when X < 1 here"
    }
    
    while { $x >= $y} {
        incr result
        set x [expr $x / $y]
    }
    
    return $result
}
#--------------------------------------------------------------------------------
# define --
#   Give chipset register a name and operator
# Arguments:
#   regname: global register name to be created
#   map: list of {operator value}  
#       Address operator is the key word for setting the register address, 
#       such as CSR:0x0017A120 and set to regname
#       other operator will be the action name with data bit range
# example:
#   define MCMTRiMC0  {Address {CSR:0x0017807C} iMCMode {12 2} LsEn {1 1} ClosePG {0 1}}
#   MCMTRiMC0 = CSR:0x0017807C
#   MCMTRiMC0.iMCMode = getRegisterEx {Reg MCMTRiMC0 Range {12 2}}
# usage:
#   [get MCMTRiMC0.iMCMode] to get CSR:0x0017807C[12:13]
#--------------------------------------------------------------------------------
proc define {regname map} {
    foreach {operator value} $map  {
        if {$operator == "Address"} {
            debug "$regname = $value"
            upvar #0 $regname temp
            set temp [uplevel subst $value]
        } else {
            set action "getRegisterEx \{Reg $regname Range \{$value\}\}"
            debug "$regname.$operator = $action "
            upvar #0 $regname.$operator temp
            set temp $action
        }
    }
}

#--------------------------------------------------------------------------------
# findLSPair --
#   Find the lockstep pair of the given dimm
# Arguments:
#   arrayRef - The name of array which constains iMC, VMSE, Ch and Dimm number.
#--------------------------------------------------------------------------------
proc findLSPair {arrayRef} {
    upvar $arrayRef dimmDetail
    
    if {![info exist dimmDetail(iMC)] || ![info exist dimmDetail(Socket)] || ![info exist dimmDetail(Channel)] || ![info exist dimmDetail(Dimm)] } {
        error "parameter is either not an array nor does not contains enough info"
    }
    
    if { [expr $dimmDetail(Channel) % 2 == 0] } {
        set counter_ch [expr $dimmDetail(Channel) + 1]
    } else {
        set counter_ch [expr $dimmDetail(Channel) - 1]
    }
    
    #debug "iMC $dimmDetail(iMC) VMSE $dimmDetail(VMSEChannel) SubCh $dimmDetail(Channel)"
    set dimmDetail(LsPlannerCh) [expr $dimmDetail(iMC) * $::CHANNEL_PER_IMC + $counter_ch]
    #return $::DIMMMap($dimmDetail(Socket)-$dimmDetail(LsPlannerCh)-$dimmDetail(Dimm))
    set dimmDetail(LsPlannerDimmNum) [findDIMMSilkNum $dimmDetail(Socket) $dimmDetail(LsPlannerCh) $dimmDetail(Dimm)]
    set dimmDetail(LsSmiPort) [expr $dimmDetail(LsPlannerCh)/2]
    set dimmDetail(LsSmiPortChannel) [expr $dimmDetail(LsPlannerCh)%2]
}

proc findDIMMSilkNum {cpu ch slot} {
    debug "cpu = $cpu ch = $ch slot = $slot"
	if {$slot == 255} {
		return 255
	}
    if {$::IsPrimary} {
        debug "Primary, return DIMMMap($cpu-$ch-$slot) = $::DIMMMap($cpu-$ch-$slot)"
        return $::DIMMMap($cpu-$ch-$slot)
    } else {
        set localCpu [expr $cpu % $::CpuPerNode]
        debug "not primary, return DIMMMap($localCpu-$ch-$slot) = $::DIMMMap($localCpu-$ch-$slot)"
        debug "cpu = $cpu CpuPerNode = $::CpuPerNode"
        return $::DIMMMap($localCpu-$ch-$slot)
    }
}

#-----------------------------------------------------------------------------------
# FromReg --
#       Extract bits from register
# Arguements:
#       startBit  extract bit start
#       bitLength bit length to be extracted
#       shift     shift left count after extracting
#       trail     trail value after shift
#       reg       register to be extracted
#-----------------------------------------------------------------------------------
proc FromReg {startBit bitLength shift trail reg} {
    debug "Entry: $reg $startBit $bitLength $shift $trail"
    set temp [eval getRegister [subst $reg] ]
    set result [getBits $temp $startBit $bitLength]
    set result [expr ($result << $shift) | $trail]
    debug "Result: [long2hex $result]"
    return $result
}

proc get {regField} {
    if {[string match *.* $regField]} {
        set action \$\{::$regField\}
        set action [subst $action]
        #debug "Going to eval $action for getting value of $regField"
        set result [eval $action]
    } else {
        set result [getRegister $regField]
    }
    debug "$regField = $result"
    return $result
}

#--------------------------------------------------------------------------------
# mapCSR --

#   Make CSR address packed as a DWORD
#--------------------------------------------------------------------------------
proc mapCSR { bus dev func reg } {
    set bus_shift [expr $bus << 20]
    set dev_shift [expr $dev << 15]
    set func_shift  [expr $func << 12]
    set addr    [expr $bus_shift | $dev_shift | $func_shift | $reg]
    debug "[format 0x%02X $bus] [format 0x%02X $dev] [format 0x%02X $func] [format 0x%03X $reg] [format 0x%08X $addr]"

    return [format 0x%08X $addr]
}

#--------------------------------------------------------------------------------
# decodeCSR --
#   Get PCI PFA (Bus/Device/Fuction/Offset) from a PCIE style DWORD.
#--------------------------------------------------------------------------------
proc decodeCSR {pfa} {
    
    set bus     [expr ($pfa >> 20) & 0xFF]
    set dev     [expr ($pfa >> 15) & 0x1F]
    set func    [expr ($pfa >> 12) & 0x07]
    set offset  [expr $pfa & 0xFFF]
    
    return [list Bus $bus Device [format %02x $dev] Function [format %02x $func] Offset [format %03x $offset]]
}

#--------------------------------------------------------------------------------
# long2hex --
#   Manually transform a 64-bit value into its hex format
#   (tcl 8.5 format 'X' does not work for 64-bit long value
#Arguments:
#   msg     the message to be output
#   args    if provided, the first value is taken as debug message level.
#--------------------------------------------------------------------------------
proc long2hex { value } {
    set msb [expr $value >> 32]
    set lsb [expr $value & 0x00000000ffffffff];
    return 0x[format %08X $msb][format %08X $lsb]
}

#-----------------------------------------------------------------------------------
# check : Given a name of register. Check if the given register has error dump value.
#         If there is, try to run script associate to that register
#-----------------------------------------------------------------------------------
proc check {name {target $::TARGET_CPU} } {
    upvar #0 $::REG_STORE REGISTERS
    set var $::PRE$name
    set exist 0

    debug "Entry Checking $name-[subst $target]"
    set varname "$var-[subst $target]"
    if { [info exist REGISTERS([subst $varname])]} then {
        set exist 1
        set value $REGISTERS([subst $varname])
        debug "$::REG_STORE\([subst $varname]\) = [long2hex $value]"
    } elseif { [info exist ::REGISTERS([subst $varname])]} then {
        set exist 1     
        set value $::REGISTERS([subst $varname])
        debug "::REGISTERS\([subst $varname]\) = [long2hex $value]"
    }

    if {$exist == 1} {
        debug "::HANDLER\($var\) = $::HANDLER([subst $var])"
        eval $::HANDLER([subst $var])
    } else {
        debug "$name has no data at all"
    }
}   

#--------------------------------------------------------------------------------
# branch : Take a register name and mask(s) and check if any bit in given mask(s)
#          is set. 
#          If true, try to excute script regitered to matched mask.
#--------------------------------------------------------------------------------
proc branch { name args {target $::TARGET_CPU}} {
    upvar #0 $::REG_STORE REGISTERS
    set params [llength $args]
    if { $params > 1 } {
        #debug "Going to check branch on $args"
        for { set i 0 } { $i < $params  } { incr i } {
            set subcond [lindex $args $i]
            branch $name $subcond
        }
        return
    } else {
        set cond [lindex $args 0]
    }

    debug "Entry: Test $name.$args on CPU[subst $target]"
    set regname "$name-[subst $target]"
    set regrealname "[subst $::PRE$name-$target]"
    #set value $REGISTERS([subst $::PRE$name]-$::TARGET_CPU)
    if { [info exist REGISTERS($regrealname)]} then {
        set value $REGISTERS($regrealname)
        set regarrayname "$::REG_STORE"
    }
    if { [info exist ::REGISTERS($regrealname)]} then {
        set value $::REGISTERS($regrealname)
        set regarrayname "::REGISTERS"
    }

    set regfullname "$regarrayname\($regname\)"
    set regfullrealname "$regarrayname\($regrealname\)"

    set condition [subst $::PRE$cond]
    debug "[subst $regfullrealname]=[format 0x%016lX $value]"
    debug "$args=[format 0x%016lX $condition]"
        
    set tbody [subst $::PRE$name-$::PRE$cond]
    set fbody [subst $::PRE$name-!$::PRE$cond]

    #debug "tbody = $tbody"
    #debug "fbody = $fbody"
    
    set hit [expr $value & $condition]
    
    if { $hit } then {
        if { [info exist [subst ::HANDLER($tbody)]] } then {
            set branchlog "$regfullname.$cond is set. $::HANDLER($tbody)" 
            debug $branchlog
            debug "\$::HANDLER\($::PRE$name-$::PRE$cond\) = $::HANDLER($tbody)"
            add_log errlog $branchlog
            eval $::HANDLER($tbody)
        } else {
            #debug "Hit but not exist"
        }
    } else {
        if { [info exist [subst ::HANDLER($fbody)]] } then {
            set branchlog "$regfullname.$cond is not set. $::HANDLER($fbody)"
            debug $branchlog
            add_log errlog $branchlog           
            eval $::HANDLER($fbody)
        } else {
            #debug "Not Hit and not exist"
        }
    }
}

#--------------------------------------------------------------------------------
# getRegisterEx --
#       Get hex value from specified name of register. It optionally get bits
#       from specified bit range, left shift given amount and padding extra value
#       with the result if correspond parameter is set.
#   Arguements:
#       params  Expected to be an even-number list which can be parsed into array.
#               The parsed array must have an element indexed as 'Reg' to indicate
#               register(s) to get. There are 3 optional paremeters: 'Range' 
#               indicates a sepcified range of bits to retrieve; 'Shift' indicates
#               left shift amount; 'Padding' represents the value the pad on.
#--------------------------------------------------------------------------------
proc getRegisterEx {params} {
    # debug "Entry: args = $params"

    #Check if params can be correctly parsed into array
    if { [catch {array set args $params} fid] } then {
        error "Parameter format error for getRegisterEx\n$fid"
    }
    
    #Check if parameters have Register name
    if { ![info exist args(Reg)] } then {
        error "Parameter contains no target register(s)"
    }
    
    #Get regigster(s) value 
    set temp [eval getRegister [subst $args(Reg)]]
    
    #debug "value=$temp"
    #Get bits in specified range if range is given
    if {[info exist args(Range)]} then {
        set range $args(Range) 
        if { [llength $range] == 2} then {
            set temp [getBits $temp [lindex $range 0] [lindex $range 1]]
        } else {
            error "Parameter 'Range' should be 2 values"
        }
        #debug "Range: getBits $temp [lindex $range 0] [lindex $range 1] = $temp"
    }

    #Left shift result value if shift amount is given
    if {[info exist args(Shift)]} then {
        set temp [expr $temp << $args(Shift)]
        #debug "Shift $args(Shift) : [long2hex $temp]"
    }
    
    #Pad lower bits value if padding value is given
    if {[info exist args(Padding)]} then {
        
        set temp [expr $temp | $args(Padding)]
        #debug "Padding [long2hex $args(Padding)]: [long2hex $temp]"
    }
    
    debug "$params = [long2hex $temp]"
    return $temp
}

#---------------------------------------------------------------------------------
# getBits --
#   Extract bits sequence from a source value starting at specified pos with offset
#   Note that this not just simply masks the given value but also right shift
#   the result.
#Arguements:
#   value    source value to be extract
#   startBit start bit of source value   
#   bits     bit number to get
#---------------------------------------------------------------------------------
proc getBits { value startBit bits } {
    
    set mask 0;
    set bit 1;
    set upper [expr $startBit + $bits]
    
    #create bit mask
    for { set i 0 } { $i < $upper } { incr i } {
        
        if { $i >= $startBit } then {
            set mask [expr $mask | $bit]
        }       
        set bit [expr $bit << 1]    
    }
    
    #return right shifted mask out value
    return [expr ($value & $mask) >> $startBit]
}


#--------------------------------------------------------------------------------
# getRegister --
#       Get hex value from specified name of register. The args can be a list.
#       If the element number of args is greater than one, data from this register
#       will be concatenated as one big-endian value.
#   Arguements:
#       args:   Register(s) name to be retrieved
#--------------------------------------------------------------------------------
proc getRegister {args} {
    #debug "entry: $args"
    
    set result 0
    
    #Do recurrsive call if args count is greater than 1
    #The result will be 4*count big endian value in hex
    if {[llength $args] > 1} then {
        for { set i 0 } { $i < [llength $args] } {incr i } {
             set result [expr $result << 32]
             set result [expr $result | [getRegister [lindex $args $i]] ]
        }
        
        set value [long2hex $result]
        debug "$args = $value" 
        return $result
    }
    
    set var $::PRE$args
    
    set idx [string first @ $var]
    if { $idx >= 0} {
        set target [subst [string range $var 0 [expr $idx - 1]]]-[string range $var [expr $idx + 1] [string length $var]]
    } else {
        set target [subst $var]-$::TARGET_CPU
    }
        
    set prefix [string range $target 0 2]
    
    if { $prefix eq "MSR" } {
        upvar #0 $::REG_STORE REGISTERS
    } else {
        upvar #0 REGISTERS REGISTERS
    }
    #debug "Get register $target"
    
    if {[info exist REGISTERS($target)]} then {
        if { $prefix eq "MSR" } {
            set value [format 0x%lX $REGISTERS($target)]
        } else {
            set value [format 0x%X $REGISTERS($target)]
        }
        
        debug "Reg:\$::$args-$::TARGET_CPU = $target = $value" 
        return $value
        #return $::REGISTERS([subst $var]-$::TARGET_CPU)
    } else {
        error "no register found for $args"
    }
}

#-----------------------------------------------------------------------------------
# checkReserved --
#       Given a system physical address, check if the address falls into one of the 
#       reserved regions address region.
# Arguements:
#       addr:   The physical address going to be checked.
#-----------------------------------------------------------------------------------
proc checkReserved { addr } {
    set revRegion [list]
    lappend revRegion LegacyVag  [list base 0xA0000     limit 0xBFFFF enable 1]
    lappend revRegion LegacyPam  [list base 0xC0000     limit 0xFFFFF enable 1]
    set baseVal   [getRegisterEx {Reg {MEBaseRegHigh MEBaseRegLow} Range {19 27} Shift 19}]
    set limitVal  [getRegisterEx "Reg {MELimitHigh MELimitLow} Range {19 27} Shift 19 Padding [expr $::BIT19 -1]"]
    set enableVal [getRegisterEx {Reg {MELimitHigh} Range {11 1}}]
    lappend revRegion MESeg      [list base $baseVal limit $limitVal enable $enableVal]
    lappend revRegion CrabAbort  [list base 0xFEB00000  limit 0xFEB0FFFF enable 1]
    lappend revRegion PSEG       [list base 0xFEB80000  limit 0xFEBFFFFF enable 1]
    lappend revRegion IOAPIC     [list base 0xFEC00000  limit 0xFECFFFFF enable 1]
    lappend revRegion LT         [list base 0xFED20000  limit 0xFED4FFFF enable 1]
    lappend revRegion ICH        [list base 0xFED00000  limit 0xFEDFFFFF enable 1]
    lappend revRegion Flash      [list base 0xFF000000  limit 0xFFFFFFFF enable 1]
    lappend revRegion NoEarlyGo  [list base 0xFE000000  limit 0xFFFFFFFF enable 1]
    
    for {set i 0} {$i <= 15} {incr i} {
        set param  [list Reg [subst "MMIORuleLow($i)"] Range {1 20} Shift 26]
        set base   [getRegisterEx $param]
        set param  [list Reg [subst "MMIORuleLow($i)"] Range {21 1} Shift 25]
        set base   [expr $base  | [getRegisterEx $param]]
        set param  [list Reg [subst "MMIORuleHigh($i) MMIORuleLow($i)"] Range {26 20} Shift 26 Padding [long2hex [expr $::BIT26-1]]]
        set limit  [getRegisterEx $param]
        set param  [list Reg [subst "MMIORuleLow($i)"] Range {24 1} Shift 25]
        set limit  [expr $limit | [getRegisterEx $param]]
        set param  [list Reg [subst "MMIORuleLow($i)"] Range {0 1}]
        set enable [getRegisterEx $param]
    
        lappend revRegion MMIO$i [list base $base limit $limit enable $enable]
    }
    
    foreach {name range} $revRegion  {
        array set cond $range
        if { $cond(enable) && $cond(base) <= $addr && $addr <= $cond(limit) } then {
            error "Memory address $addr falls into $name region.($cond(base) : $cond(limit))"
        }
    }
    
    return 0
}

#-----------------------------------------------------------------------------------
# forwardSAD --
#       Perform forward System Address Decoding. Given a system physical address, the 
#       procedure return the socket it belongs to.
# Arguements:
#       addr:   The physical address going to be decoded.
#-----------------------------------------------------------------------------------
proc forwardSAD {addr} {
    set limit 0
    set prevLimit 0
    set sad_index -1
    array set node {}
    
    #Check if address falls into any reserved region
    if {[catch {checkReserved $addr} checked]} {    
        error [subst "Forward SAD failed: $checked"]
    }
        
    #DRAM Rules
    for {set i 0} {$i < $::DECODE_ENTRIES(SAD_DRAM)} {incr i} {
        set DRAMRule($i-enable)     [getRegisterEx [subst {Reg DRAMRule($i) Range {0 1}} ]]
        set DRAMRule($i-attribute)  [getRegisterEx [subst {Reg DRAMRule($i) Range {2 2}} ]]
        set DRAMRule($i-interleave) [getRegisterEx [subst {Reg DRAMRule($i) Range {1 1}} ]]
        set DRAMRule($i-limit)      [getRegisterEx [subst {Reg DRAMRule($i) Range {6 20} Shift 26 Padding [long2hex [expr $::BIT26-1]]} ]]
        set DRAMRule($i-A7Mode)     [getRegisterEx [subst {Reg DRAMRule($i) Range {26 1}} ]]        
    #   debug "DRAM Rule $i -- Enable: $DRAMRule($i-enable) Limit:[long2hex $DRAMRule($i-limit)] Attribute:$DRAMRule($i-attribute) Interleave: $DRAMRule($i-interleave) "
    }
    
    #Find which DRAM rule matches the address
    for {set i 0} {$i < $::DECODE_ENTRIES(SAD_DRAM)} {incr i} { 
        set limit $DRAMRule($i-limit)
        
        if {$limit == $prevLimit} then {
            error "Forward SAD failed: No matched DRAM rule found"
        }
        
        if {$DRAMRule($i-enable) == 1 && $addr < $limit} then {
            set sad_index $i
            break
        }
        
        set prevLimit  $limit
    }
    
    #Check if no rule has a match
    if {$sad_index < 0} then {
        error "Forward SAD failed: No matched DRAM rule found"
    }
    
    #Check if address does not map to coherence memory 
    if {$DRAMRule($sad_index-attribute) != 0} then {
        error "Forward SAD failed: Address not mapped to coherence memory"
    }
    
    #debug "DRAMRule matched is $sad_index, interleave =  $DRAMRule($sad_index-interleave), A7Mode = $DRAMRule($sad_index-A7Mode)"
    
    #Calculate interleave index for A7/A6 index mode
    if { $DRAMRule($sad_index-A7Mode) } then {
        set lowerBits [expr ((($addr >> 7) & 0x03) << 1)]
        set lowerBits [expr $lowerBits | (($addr >> 9) & 0x01)]
        #debug "A7Mode: lowerBits: \[8, 7, 9\] = $lowerBits"
    } else {
        set lowerBits [expr ($addr >> 6) & 0x07]
        #debug "A6Mode: lowerBits: \[8, 7, 6\] = $lowerBits"
    }
    
    #Calculate node interleave index
    #   If DRAM rule interleave flag is set to 1, the interleave index is calculated as bit[6:8] XOR bit[16:18].
    #   Otherwise, the index is bit[6:8]
    if { $DRAMRule($sad_index-interleave) == 0} then {
        set xorRowBits [expr (($addr >> 16) & 0x07)]
        set interleave_list_index [expr $lowerBits ^ $xorRowBits]
    } else {
        set interleave_list_index $lowerBits
    }
    
    #debug "Interleave index = $interleave_list_index and its Interleave list is [long2hex [getRegisterEx [subst {Reg {InterleaveList($sad_index)}} ]]]"
    
    #Get node id
    set nodeId [getRegisterEx [subst {Reg {InterleaveList($sad_index)} Range {[expr $interleave_list_index * 4] 4} }]]

    #nodeID[3:0] = SASS
    #where S is socket id
    #A is agent id.  HA0 has Agent Id 0.  HA1 has 1
    #CA has only one agent id which is 0
    set socket [expr (($nodeId &  $::BIT3) >> 1) | ($nodeId & 0x03) ]
    set iMC    [expr ($nodeId >> 2) & 0x01 ]
    
    set result [list Socket $socket iMC $iMC]
    #debug "SAD result=$result"
    return $result
}

#-----------------------------------------------------------------------------------
# forwardTAD --
#       Perform forward Target Address Decoding. Given a system physical address, socket
#       and iMC, the procedur returns the channel it belongs to and its channel address.
# Arguements:
#       addr:   The physical address going to be decoded.
#       socket: The socket where the given system address belongs to
#       iMC:    The iMC where the given system addrss belongs to
#-----------------------------------------------------------------------------------
proc forwardTAD { addr socket iMC } {
    set tadIdx -1
    set limit 0
    set prevLimit 0
    set cpuSilk [expr $socket + 1]
    # debug "forwardTAD [long2hex $addr] $socket $iMC"
    #TAD Segment
    for {set i 0} {$i < $::DECODE_ENTRIES(TAD)} {incr i} {
        # 3.1.19 TAD[0:11]
        # TAD Limit (limit)
        set TAD($iMC-$i-limit)      [FromReg 12 20 26 [long2hex [expr $::BIT26-1]] TAD($iMC-$i)@$cpuSilk]
        # Number of Socket Ways (socketways)
        set TAD($iMC-$i-socketWay)  [FromReg 10 2 0 0 TAD($iMC-$i)@$cpuSilk]    
        # Number of Channel Ways (channelways)
        set TAD($iMC-$i-channelWay) [FromReg 8 2 0 0 TAD($iMC-$i)@$cpuSilk]
        #debug "TAD $i -- Limit:[long2hex $TAD($iMC-$i-limit)]   SocketWay:$TAD($iMC-$i-socketWay)  ChannelWay: $TAD($iMC-$i-channelWay)"
        # tad_ch_tgt##interleave
        # TAD_CHANNEL_TARGET (channel_3_id):
        # TAD_CHANNEL_TARGET (channel_2_id):
        # TAD_CHANNEL_TARGET (channel_1_id):
        # TAD_CHANNEL_TARGET (channel_0_id):
        # Logical channel interleave 0/1/2/3 to Physical channel ID mapping.
        for {set j 0} {$j < 4} {incr j} {
            set offset [expr $j * 2]
            set TAD($iMC-$i-channelId-$j) [FromReg $offset 2 0 0 TAD($iMC-$i)@$cpuSilk]
        }
    }   
    
    #Search for suitable TADWayness
    for {set i 0} {$i < $::DECODE_ENTRIES(TAD)} {incr i} {
        set limit $TAD($iMC-$i-limit)
        #debug "addr=[long2hex $addr] limit=[long2hex $limit]  preLimit=$prevLimit"
        
        if {$limit == $prevLimit} then {
            error "Forward TAD failed: No matched TAD wayness is found."
        }
        
        if {$addr <= $limit} then {
            set tadIdx $i
            break
        }
        
        set prevLimit $limit
    }
    
    if {$tadIdx < 0 } then {
        error "Forward TAD failed: No matched TAD wayness is found."
    }
    
    #Prapare info to find channel index and address
    set sktWay  [pow 2 $TAD($iMC-$tadIdx-socketWay)]
    set chWay   [expr $TAD($iMC-$tadIdx-channelWay) + 1]
    set sktXch  [expr $sktWay * $chWay]
    set imcCW   [expr [FromReg 8 2 0 0 TADWayness($iMC-$tadIdx)@$cpuSilk] + 1]
    
    #debug "TAD index matched: $tadIdx with sktWay: $sktWay, chWay: $chWay, imcCW: $imcCW" D_VERBOSE
    
    set MCChanShiftUpEnable [get [subst HADefeature2iMC$iMC.MCChanShiftUpEnable]]
    set MCChanHashEn        [get [subst HADefeature2iMC$iMC.MCChanHashEn]]
    set MirrorEn            [get HADefeature.MirrorMod]
	set splitBit            [expr 6 + $MCChanShiftUpEnable]
    
    if {$chWay == 3} {
        set chIntlv [expr ($addr >> 6) % $chWay]
    } else {
	    if {$::CPUTYPE eq "CPU_IVT"} {
		    set chIntlv [expr ($addr >> (6 + $TAD($iMC-$tadIdx-socketWay) + $MCChanShiftUpEnable)) & 0x3] 
        } elseif {$::CPUTYPE eq "CPU_HSX"} {
		    set chIntlv [expr ($addr >> ($splitBit + $TAD($iMC-$tadIdx-socketWay))) & 0x3] 
	    }

        if {$MCChanHashEn} {
            #Hash_ChannelID[0] = ChannelID[0] ^ (A12 ^ A14 ^ A16 ^ A18 ^ A20 ^ A22 ^ A24 ^A26) AND CSR_hash_enable
            #Hash_ChannelID[1] = ChannelID[1] ^ (A13 ^ A15 ^ A17 ^ A19 ^ A21 ^ A23 ^ A25 ^A27) AND CSR_hash_enable
            #debug "MC Channel Hash enabled" D_VERBOSE
            foreach {entry} $::chXorList {
                set chIntlv [expr $chIntlv ^ [getBits $addr $entry 1]]
            }
        }
        set chIntlv [expr $chIntlv % $chWay]
        #Get logic interleave channel id from tad_ch_tgt
        set channel $TAD($iMC-$tadIdx-channelId-$chIntlv)

	    if {$::CPUTYPE eq "CPU_IVT"} {
		    if {$MirrorEn} {
			    # 3.1.26 HACHFAILSTS
			    # Fail of Memory Channel 0 (FailCh0)
			    # Fail of Memory Channel 1 (FailCh1)
			    # Fail of Memory Channel 2 (FailCh2)
			    # Fail of Memory Channel 3 (FailCh3)	
			    for {set i 0} {$i < 4} {incr i} {
				    set HACHFAILSTS($iMC-FailCh-$i) [FromReg $i 1 0 0 HACHFAILSTS($iMC)]
			    }

			    #1-way interleave across mirror pairs
			    if {$chWay == 2} {
				    if { $chIntlv == 0 } {
					    if { $HACHFAILSTS($iMC-FailCh-$channel) == 1} {
						    set channel TAD($iMC-$tadIdx-channelId-1)
					    } else {
						    set channel_2nd TAD($iMC-$tadIdx-channelId-1)
					    }
				    } elseif { $chIntlv == 1 } {
					    if { $HACHFAILSTS($iMC-FailCh-$channel) == 1} {
						    set channel TAD($iMC-$tadIdx-channelId-0)
					    } else {
						    set channel_2nd TAD($iMC-$tadIdx-channelId-0)
					    }					
				    } else {
					    error "Forward TAD failed: unexpected Channel Interleave value in 1-way mirror mode" 
				    }
			    } elseif {$chWay == 4} {
				    if { $chIntlv == 0 } {
					    if { $HACHFAILSTS($iMC-FailCh-$channel) == 1} {
						    set channel TAD($iMC-$tadIdx-channelId-2)
					    } else {
						    set channel_2nd TAD($iMC-$tadIdx-channelId-2)
					    }
				    } elseif { $chIntlv == 1 } {
					    if { $HACHFAILSTS($iMC-FailCh-$channel) == 1} {
						    set channel TAD($iMC-$tadIdx-channelId-3)
					    } else {
						    set channel_2nd TAD($iMC-$tadIdx-channelId-3)
					    }					
				    } elseif { $chIntlv == 2 } {
					    if { $HACHFAILSTS($iMC-FailCh-$channel) == 1} {
						    set channel TAD($iMC-$tadIdx-channelId-0)
					    } else {
						    set channel_2nd TAD($iMC-$tadIdx-channelId-0)
					    }
				    } elseif { $chIntlv == 3 } {
					    if { $HACHFAILSTS($iMC-FailCh-$channel) == 1} {
						    set channel TAD($iMC-$tadIdx-channelId-1)
					    } else {
						    set channel_2nd TAD($iMC-$tadIdx-channelId-1)
					    }
				    } else {
					    error "Forward TAD failed: unexpected Channel Interleave value in 2-way mirror mode" 
				    }
			    } else {
				    error "Forward TAD failed: channelways set to illegal value for mirroring mode."
			    }
		    }
        } elseif {$::CPUTYPE eq "CPU_HSX"} {
	        set PtlMirEn		[get [subst HADefeature2iMC$iMC.PtlMirEn]]

			#Not mirrored
		    if {$MirrorEn == 0 || ($PtlMirEn ==1 && $tadIdx != 0)} {
			    if {$chWay != $imcCW} then {
				    error "Forward TAD failed: Bad Memory Configuration."
				}
			} else {
			    # 3.1.37 HACHFAILSTS
			    # Fail of Memory Channel 0 (FailCh0)
			    # Fail of Memory Channel 1 (FailCh1)
			    # Fail of Memory Channel 2 (FailCh2)
			    # Fail of Memory Channel 3 (FailCh3)	
			    for {set i 0} {$i < 4} {incr i} {
				    set HACHFAILSTS($iMC-FailCh-$i) [FromReg $i 1 0 0 HACHFAILSTS($iMC)]
			    }

			    #1-way interleave across mirror pairs
			    if {$chWay == 2} {
				    debug "forwardTAD : 1-way channel interleaving across mirror pairs."
				} elseif {$chWay == 4} {
			        debug "forwardTAD : 2-way channel interleaving across mirror pairs."
				} else {
				    error "Forward TAD failed: channelways set to illegal value for mirroring."
				}

				set secCh [expr $channel ^ $MirrorEn]

				if {$HACHFAILSTS($iMC-FailCh-$channel) == 1} {
				    set channel $secCh
				} else {
				    set channel_2nd $secCh
                }
			}
		}
	}
			
	if {$::CPUTYPE eq "CPU_IVT"} {
	    #debug "MCMTR [get MCMTR]  MCMTR.LsEn [get MCMTR.LsEn]  MCMTR.iMCMode  [get MCMTR.iMCMode]" D_VERBOSE
	    set LsEn 	[get [subst MCMTRiMC$iMC.LsEn]]
	    set iMCMode [get [subst MCMTRiMC$iMC.iMCMode]]
	    #set splitBit [expr 6 + $MCChanShiftUpEnable]
	
	    # Adjust channel number if lockstep is enabled
	    # Lackstep or VMSE 1:1 subchannel Lockstep Mode
	    if {$LsEn == 1 || $iMCMode == 2} {
            debug "LockStep mode"
		    set ls_en 1
		    if {[getBits $addr 3 1] == 1} {
			    set $channel [expr $channel + 1]
			    if {$MirrorEn} {
				    set channel_2nd [expr $channel_2nd+1]
			    }
		    }
	    } else {
		    set ls_en 0
	    }
	
	    #Get the channel offset
	
	    set offsetReg iMC${iMC}Ch${channel}TADCHNILVOFFSET${tadIdx}
	    # TAD_OFFSET (tad_offset) in 64 MB granularity
	    set chOffset [expr [get $offsetReg.TadOffset] << 26]

	    set bitsRemoved [log $sktXch 2]

	    debug "bitsRemoved: $bitsRemoved"

	    if {$MCChanShiftUpEnable == 1 && [get $offsetReg.A6Shift] != $bitsRemoved} {
		    error "a6_shift set to illegal value"
	    }
    } elseif {$::CPUTYPE eq "CPU_HSX"} {
	    #debug "MCMTR [get MCMTR]  MCMTR.LsEn [get MCMTR.LsEn]  MCMTR.iMCMode  [get MCMTR.iMCMode]" D_VERBOSE
	    set LsEn 	[get [subst MCMTRiMC$iMC.LsEn]]
	    set iMCMode [get [subst MCMTRiMC$iMC.iMCMode]]
		set a3 [expr ($addr >> 3) & 1]

	    #Get the channel offset
	
	    set offsetReg iMC${iMC}Ch${channel}TADCHNILVOFFSET${tadIdx}
	    # TAD_OFFSET (tad_offset) in 64 MB granularity
	    set chOffset [expr [get $offsetReg.TadOffset] << 26]

        #VMSE1:1 Subchannel Lockstep Mode decodes from the even/logcal channel
		if {$iMCMode == 2} {
            debug "LockStep mode"
	        set ls_en 1
            if {$a3 == 1} {
		        set channel [expr $channel + 1]
			    if {$MirrorEn} {
				    if {$PtlMirEn ==1 && $tadIdx != 0 } {
	                    #pass
                    } else {
				        set channel_2nd [expr $channel_2nd+1]
			        }
				}
		    }
		} else {
		    set ls_en 0
	    }

		if {$sktXch != 1} {
	        set bitsRemoved [log $sktXch 2]

	        debug "bitsRemoved: $bitsRemoved"
		}
	}

    #debug "System address [long2hex $addr]" D_VERBOSE
    set chAddr [expr $addr - $chOffset]
    #debug "System address - Channel offset: [long2hex $chAddr]" D_VERBOSE
    #set chAddr [expr $chAddr >> (6 + [get HADefeature2.MCChanShiftUpEnable])]
    set chAddr [expr $chAddr >> $splitBit]
    #debug "Channel address after shift right [long2hex $chAddr]"    D_VERBOSE
    set chAddr [expr $chAddr / $sktWay]
    #debug "Channel address / $sktWay [long2hex $chAddr]" D_VERBOSE
    set chAddr [expr $chAddr / $imcCW]
    #debug "Channel address / $imcCW [long2hex $chAddr]" D_VERBOSE
    set chAddr [expr ($chAddr << $splitBit) | [getBits $addr 0 $splitBit]]
    #debug "Channel address after shift left [long2hex $chAddr]" D_VERBOSE
    
    set VMSEChannel [expr $channel / 2]
    set chAddrHex   [long2hex $chAddr]
    
    set result [list Channel $channel ChannelAddress $chAddrHex VMSEChannel $VMSEChannel]

    if {[info exist channel_2nd]} {
        lappend result channel_2nd $channel_2nd
    }

    if {[info exist ls_en]} {
        lappend result ls_en $ls_en
    }
    
    #debug "TAD result=$result" D_INFO
    return $result
}

#-----------------------------------------------------------------------------------
# forwardRIR --
#       Perform forward Rank Interleave Decoding. Given a channel, channel address, socket
#       and iMC, the procedure returns the dimm it belongs to and its rank address.
# Arguements:
#       socket: The socket where the given address belongs to
#       iMC:    The iMC where the given addrss belongs to
#       channel:The channel where the give address belongs to
#       chAddr: The channel address going to be decoded.    
#       
#-----------------------------------------------------------------------------------
proc forwardRIR {Socket iMC channel chAddr} {
    set prevLimit 0
    set cpuSilk [expr $Socket + 1]
    
    #debug "Forward RIR for Socket: $Socket, iMC: $iMC, Channel: $channel, Channel Address: $chAddr"
    # 5.3.22 RIRWAYNESSLIMIT_[0:4]
    for {set i 0} {$i < $::CHANNEL_PER_IMC} {incr i} {
            for {set j 0} {$j < $::DECODE_ENTRIES(RIR)} {incr j} {
            # RIR_VAL (rir_val):
            set RIPWayness($i-$j-val) [FromReg 31 1 0 0 RIR($iMC-$i-$j)@$cpuSilk]
            # RIR_WAY (rir_way):
            set RIPWayness($i-$j-way) [FromReg 28 2 0 0 RIR($iMC-$i-$j)@$cpuSilk]
            # RIR_LIMIT (rir_limit):
            set RIPWayness($i-$j-limit) [FromReg 1 10 29 [long2hex [expr $::BIT29-1]] RIR($iMC-$i-$j)@$cpuSilk]
        }
    }
    
    #debug "using channel $channel to decode rip for $channelAddress" 
    
    #Search for matching RIR Segment
    set previous_limit -1
    set match 0
    #debug "Channel Address = [long2hex $chAddr]"
    for {set rir_index 0} {$rir_index < $::DECODE_ENTRIES(RIR)} {incr rir_index} {
        set limit $RIPWayness($channel-$rir_index-limit)
        set valid $RIPWayness($channel-$rir_index-val)  
        #debug "limit = [long2hex $limit], $valid"
    
        if { $valid != 1} {
            continue
        }
        if {$rir_index != 0 && $limit == $prevLimit} then {
            break;
        }
        
        if {$previous_limit < $chAddr && $chAddr <= $limit} then {
            set match 1
            break
        }
        
        set prevLimit $limit
    }

    if {$match == 0} then {
        error "Forward RIR failed: No RIR match is found"
    }

    set rirWay  [pow 2 $RIPWayness($channel-$rir_index-way)]

    set ClosePG [get [subst MCMTRiMC$iMC.ClosePG]]
    
    if {$ClosePG} {
        # Closed Page Address Mapping Mode
        set rankIntlv   [expr ($chAddr >> 6) % $rirWay]
        set rankAddr    [expr ($chAddr >> 6) / $rirWay]
        set rankAddr    [expr ($rankAddr << 6) | [getBits $chAddr 0 6]]
    } else {
        # Open/Adaptive Page Address Mapping Mode
        set rankIntlv   [expr ($chAddr >> 13) % $rirWay]
        set rankAddr    [expr ($chAddr >> 13) / $rirWay]
        set rankAddr    [expr ($rankAddr << 13) | [getBits $chAddr 0 13]]
    }

    set rirOffsetReg  iMC${iMC}Ch${channel}RIRILV${rankIntlv}OFFSET${rir_index}
    set chipSel     [get $rirOffsetReg.RirRankTgt]
    set rirOffset   [get $rirOffsetReg.RirOffset]
    set dimm [expr $chipSel / 4]
    set physRank [expr $chipSel % 4]
    set rankAddr [expr $rankAddr - $rirOffset]
    set rankAddrHex [long2hex $rankAddr]
    
    set Result [list Dimm $dimm RankAddress $rankAddrHex PhysRank $physRank]
    #debug "RIR result: $Result"
    return $Result
}

#-----------------------------------------------------------------------------------
# forwardMad --
#    Forward mad address algorithm - Takes in a TranslationInfo class and maps
#    the ti.rank_addr (36-bit rank address) to a ti.bank, ti.row, and ti.column.
#    The verbose flag signals if the function is being used without the
#    console "gui" in which case it will print off a little more information.
# Arguements:
#       socket: The socket where the given address belongs to,  0-3
#       iMC:    The iMC where the given addrss belongs to,      0-1
#       channel:The channel where the give address belongs to,  0-3
#       chAddr: The channel address going to be decoded.        
#       RankAddr: 
#       
#-----------------------------------------------------------------------------------
proc forwardMad {Socket iMC channel rank_addr PhysRank Dimm} {
    #mcmtr
    set ls_en   [get [subst MCMTRiMC$iMC.LsEn]]
    set imc_mode [get [subst MCMTRiMC$iMC.iMCMode]]
    set close_pg [get [subst MCMTRiMC$iMC.ClosePG]]
    set ddr4 [get [subst MCMTRiMC$iMC.ddr4]]
    set bank_xor_enable [get [subst MCMTRiMC$iMC.bank_xor_enable]]

    #HADefecture
    set LockstepEn [get HADefeature.LockstepEn]

    #amap
    set hsxplus [get iMC${iMC}_Ch${channel}_amap.hsxplus]

    #dimmmtr
    set dimm_pop [get iMC${iMC}_Ch${channel}_dimmmtr_${Dimm}.dimm_pop]
    set rank_disable [get iMC${iMC}_Ch${channel}_dimmmtr_${Dimm}.rank_disable]
    set ra_width [get iMC${iMC}_Ch${channel}_dimmmtr_${Dimm}.ra_width]
    set ca_width [get iMC${iMC}_Ch${channel}_dimmmtr_${Dimm}.ca_width]
    set ddr4_mode [get iMC${iMC}_Ch${channel}_dimmmtr_${Dimm}.ddr4_mode]
    set ddr4_3dsnumranks_cs [get iMC${iMC}_Ch${channel}_dimmmtr_${Dimm}.ddr4_3dsnumranks_cs]
    
    if {$::EX_SUPPORT} {
        if {$imc_mode == 2} {
            debug "LockStep mode"
            set lockStep 1
        } else {
            set lockStep 0
        }
    } else {
        set lockStep $ls_en
    }


    if {$::EX_SUPPORT && $lockStep && [expr $channel%2]} {
        set evenChannel [expr $channel -1]
    } else {
        set evenChannel $channel
    }

    set column 0
    if {$ddr4} {
        if {$close_pg} {
            debug "DDR4 Closed Page Address Mapping Mode"
            #build the bank group
            set bank_group [getBits $rank_addr 6 2]
            #build the bank
            set bank [getBits $rank_addr 8 2]
            if {$bank_xor_enable} {
                debug "bank xoring enabled"
                set bank_group [expr $bank_group ^ [getBits $rank_addr 20 2]]
                set bank [expr $bank ^ [getBits $rank_addr 22 1]]
                set bank [expr $bank ^ ([getBits $rank_addr 28 1] << 1)]
            }
            #build the column
            #c0, c1, c2 depends on lockstep
            if {$lockStep} {
                #c0, c1
                set column [expr $column | [getBits $rank_addr 4 2]]
                #c2
                set column [expr $column | ([getBits $rank_addr 29 1] << 2)]
            } else {
                #c0, c1, c2
                set column [expr $column | [getBits $rank_addr 3 3]]
            }
            #c3
            set column [expr $column | ([getBits $rank_addr 14 1] << 3)]
            #c4
            set column [expr $column | ([getBits $rank_addr 19 1] << 4)]            
            #c5, c6, c7, c8, c9
            set column [expr $column | ([getBits $rank_addr 23 5] << 5)]            
            #c10 is the autoprecharge bit and will always be set in closed page            
            set column [expr $column | (1 << 10)]            

            #build the row
            #r0, r1, r2, r3
            set row [getBits $rank_addr 15 4]
            #r4, r5, r6
            set row [expr $row | ([getBits $rank_addr 20 3] << 4)]            
            #r7
            set row [expr $row | ([getBits $rank_addr 28 1] << 7)]
            #r8, r9, r10, r11
            set row [expr $row | ([getBits $rank_addr 10 4] << 8)]
            #r12 depends on lockstep (causes r12-r17 to be shifted up 1 in the rank address space)
            set row [expr $row | ([getBits $rank_addr [expr 29+$lockStep] 1] << 12)]
            #r13, r14, r15, r16, r17 handled below
        } else {
            debug "DDR4 Open/Adaptive Page Address Mapping Mode"
            #build the bank group
            if {$hsxplus} {
                set bank_group [getBits $rank_addr 6 1]
            } else {
                set bank_group [getBits $rank_addr 13 1]            
            }
            set bank_group [expr $bank_group | ([getBits $rank_addr 17 1] << 1)]
            #7-1-14 bank_xor_enable is now POR and will be in eds xml
            if {$bank_xor_enable} {
                debug "  bank_group before xor: 0x[format %X $bank_group]"
                set bank_group [expr $bank_group ^ [getBits $rank_addr 20 2]]
                debug "  bank_group after  xor: 0x[format %X $bank_group]"                
            }

            #build the bank
            set bank [getBits $rank_addr 18 2]
            #7-1-14 bank_xor_enable is now POR and will be in eds xml
            if {$bank_xor_enable} {
                debug "  bank before xor: 0x[format %X $bank]"
                set bank [expr $bank ^ [getBits $rank_addr 22 2]]
                debug "  bank after  xor: 0x[format %X $bank]"                
            }

            #build the column
            #c0-c8 depends on lockstep
            if {$lockStep} {
                #c0, c1
                set column [expr $column | [getBits $rank_addr 4 2]]
                #c2
                set column [expr $column | ([getBits $rank_addr 29 1] << 2)]
            } else {
                #c0, c1, c2
                set column [expr $column | [getBits $rank_addr 3 3]]
            }
            #c3, c4, c5, c6, c7, c8, c9
            set column [expr $column | ([getBits $rank_addr [expr 6 + $hsxplus] 7] << 3)]

            #c10 is the autoprecharge bit and wont be used in open page mode

            #build the row
            #r0, r1, r2
            set row [getBits $rank_addr 14 3]
            #r3
            set row [expr $row | ([getBits $rank_addr 20 1] << 3)]
            #r4
            set row [expr $row | ([getBits $rank_addr 28 1] << 4)]   
            #r5, r6, r7, r8, r9, r10, r11
            set row [expr $row | ([getBits $rank_addr 21 7] << 5)]
            #r12 depends on lockstep (causes r12-r17 to be shifted up 1 in the rank address space)
            set row [expr $row | ([getBits $rank_addr [expr 29+$lockStep] 1] << 12)]
            #r13, r14, r15, r16, r17 handled below
        } 
    } else {
        if {$close_pg} {
            debug "DDR3 Closed Page Address Mapping Mode"
            
            #build the bank group
            set bank_group [getBits $rank_addr 6 2]
            #build the bank
            set bank [getBits $rank_addr 8 2]
            #7-1-14 bank_xor_enable is now POR and will be in eds xml
            if {$bank_xor_enable} {
                debug "bank xoring enabled"
                set bank [expr $bank ^ [getBits $rank_addr 19 3]]
            }
            
            #build the column
            #c0, c1, c2 depends on lockstep
            if {$lockStep} {
                #c0, c1
                set column [expr $column | [getBits $rank_addr 4 2]]
                #c2
                set column [expr $column | ([getBits $rank_addr 28 1] << 2)]
            } else {
                #c0, c1, c2
                set column [expr $column | [getBits $rank_addr 3 3]]
            }
            #c3
            set column [expr $column | ([getBits $rank_addr 13 1] << 3)]
            #c4
            set column [expr $column | ([getBits $rank_addr 18 1] << 4)]            
            #c5, c6, c7, c8, c9
            set column [expr $column | ([getBits $rank_addr 22 5] << 5)]     
            #c10 is the autoprecharge bit and will always be set in closed page            
            set column [expr $column | (1 << 10)]            
            #c11 handled below
            #c12 is 1 for BL8 (Independent mode) and 0 for BL4 (Lockstep mode) when in burst chop
            #which haswell does not support, so it will always be 0
            #c13 depends on dimm technology and lockstep
            set column [expr $column | ([getBits $rank_addr [expr 33+$lockStep] 1] << 13)]
            
            #build the row
            #r0, r1, r2, r3
            set row [getBits $rank_addr 14 4]
            #r4, r5, r6
            set row [expr $row | ([getBits $rank_addr 19 3] << 4)]            
            #r7
            set row [expr $row | ([getBits $rank_addr 27 1] << 7)]
            #r8, r9, r10, r11
            set row [expr $row | ([getBits $rank_addr 9 4] << 8)]
            #r12 depends on lockstep (causes r12-r17 to be shifted up 1 in the rank address space)
            set row [expr $row | ([getBits $rank_addr [expr 28+$lockStep] 1] << 12)]
            #r13, r14, r15, r16, r17 handled below
        } else {
            debug "DDR3 Open/Adaptive Page Address Mapping Mode"
            
            #build the bank
            #b0            
            set bank [getBits $rank_addr 13 1]
            #b1, b2
            set bank [expr $bank | ([getBits $rank_addr 17 2] << 1)]
            #7-1-14 bank_xor_enable is now POR and will be in eds xml
            if {$bank_xor_enable} {
                debug "  bank before xor: 0x[format %X $bank]"
                set bank [expr $bank ^ [getBits $rank_addr 19 3]]
                debug "  bank after  xor: 0x[format %X $bank]"                
            }

            #build the column
            #c0-c8 depends on lockstep
            if {$lockStep} {
                #c0, c1
                set column [expr $column | [getBits $rank_addr 4 2]]
                #c2
                set column [expr $column | ([getBits $rank_addr 28 1] << 2)]
                #c3-c9
                set column [expr $column | ([getBits $rank_addr 6 7] << 3)]
            } else {
                #c0, c1, c2, c3, c4, c5, c6, c7, c8, c9
                set column [expr $column | [getBits $rank_addr 3 10]]
            }
            #c10 is the autoprecharge bit and wont be used in open page mode
            #c11 is handled below
            #c12 is 1 for BL8 (Independent mode) and 0 for BL4 (Lockstep mode) when in burst chop
            #which haswell does not support, so it will always be 0
            #c13 depends on dimm technology and lockstep
            if {$ca_width > 1} {
                set c13 [getBits $rank_addr [expr 33 + $lockStep] 1]
                set column [expr $column | ($c13 << 13)]
                if {$c13} {
                    debug "Found c13 set"
                }
            }

            #build the row
            #r0, r1, r2
            set row [getBits $rank_addr 14 3]
            #r3
            set row [expr $row | ([getBits $rank_addr 19 1] << 3)]      
            #r4
            set row [expr $row | ([getBits $rank_addr 27 1] << 4)]   
            #r5, r6, r7, r8, r9, r10, r11
            set row [expr $row | ([getBits $rank_addr 20 7] << 5)]
            #r12 depends on lockstep (causes r12-r17 to be shifted up 1 in the rank address space)
            set row [expr $row | ([getBits $rank_addr [expr 28+$lockStep] 1] << 12)]
            #r13, r14, r15, r16, r17 handled below
        }
    }

    #upper level row bits mapped the same across open and closed page mode
    #r13, r14, r15, r16, r17 depends on dimm technology and lockstep
    set lowPos [expr 29 + $lockStep + $ddr4]
    set highPos [expr 29 + $lockStep + $ddr4 + $ra_width-2]
    if {$ca_width > 0} {
        #c11
        set column [expr $column | ([getBits $rank_addr $lowPos 1] << 11)]

        #c11 is using r13 position, so the row gets bumped up by 1
        incr lowPos
        incr highPos
    }
    set row [expr $row | ([getBits $rank_addr $lowPos [expr $highPos - $lowPos + 1]] << 13)]
    debug "ra_width ($ra_width) = [expr 12+$ra_width] row bits so row\[17:13\] (0x[format %X [getBits $row 13 5]]) is at bits $lowPos - $highPos"

    #build the chip_id
    if {$ddr4_3dsnumranks_cs != 0} {
        incr lowPos
        incr highPos [expr $ddr4_3dsnumranks_cs -1]
        set chip_id [getBits $rank_addr $lowPos [expr $highPos - $lowPos + 1]]
        debug "ddr4_3dsnumranks_cs ($ddr4_3dsnumranks_cs) = [expr 2**$ddr4_3dsnumranks_cs] ranks/CS so c\[2:0\] is at bits $lowPos - $highPos"
    } else {
        set chip_id 0
    }

    set leftOver [expr $rank_addr >> ($highPos + 1)]
    if {$leftOver != 0} {
        error "ForwardMad failed: Something wrong withleftover."
    }

    set Result [list chip_id [format %X $chip_id] bank_group [format %X $bank_group] bank [format %X $bank] row [format %X $row] column [format %X $column]]
    #debug "Mad result: $Result"
    return $Result
}

#-----------------------------------------------------------------------------------
# forwardXlator --
#       Perform forward address translation. Given a system physical address, the 
#       procedur return its socket, channel, rank and slot silk number.
# Arguements:
#       addr:   The physical address going to be decoded.
#-----------------------------------------------------------------------------------
proc forwardXlator { addr } {
    debug "Translating address: [long2hex $addr]" 
    array set result {}
    set result(SystemAddress) [long2hex $addr]
    debug "Result init=[array get result]"
    
    #results in nodeID, and socket
    array set sad [forwardSAD $addr]
    array set result [concat [array get result] [array get sad]]
    debug "Result after forwardSAD:[array get result]"

    #results in logical channel address
    array set tad [forwardTAD $addr $result(Socket) $result(iMC)]
    array set result [concat [array get result] [array get tad]]
    debug "Result after forwardTAD:[array get result]"
    
    #results in rank address
    array set rip [forwardRIR $result(Socket) $result(iMC) $result(Channel) $result(ChannelAddress)]
    array set result [concat [array get result] [array get rip]]
    debug "Result after forwardRIR:[array get result]" 

    #results in BankGroup/Bank/Row
    array set mad [forwardMad $result(Socket) $result(iMC) $result(Channel) $result(RankAddress) $result(PhysRank) $result(Dimm)] 
    array set result [concat [array get result] [array get mad]]
    debug "Result after forwardMad:[array get result]" 

    return [array get result]
}

#-----------------------------------------------------------------------------------
# FindFaultDIMM_JCK --
#       Find the faulty DIMM according record logged in LOGDDRPARITY in JCK.
# Arguements:
#       cpu:    CPU to check
#       ha:     HA(Home Agent) to check
#       ch:     Channel to check
#-----------------------------------------------------------------------------------
proc FindFaultDIMM_JCK {cpu ha} {
    debug "Finding faulty DIMM(JCK) on socket:$cpu HA:$ha"
    set jck 0
    set ddr 0
    set ErrFound 0

    set ReadEccError 0
    
    #Check FERR
    for {set jck 0} {$jck < 2} {incr jck} {
        set pre     Ha${ha}Jck${jck}FErr
        set Ddr0Parity [get $pre.Ddr0Parity]
        set Ddr1Parity [get $pre.Ddr1Parity]
        # don't use ReadEccError as UCE
        # set ReadEccError [get $pre.ReadEccError]
        if {$Ddr0Parity == 1 || $Ddr1Parity == 1 || $ReadEccError == 1} {
            debug "error found in FERR of HA${ha}JCK${jck}"
            set ErrFound 1
            break
        }
    }

    #Check NERR
    if {$ErrFound != 1} {
        for {set jck 0} {$jck < 2} {incr jck} {
            set pre     Ha${ha}Jck${jck}NErr
            set Ddr0Parity [get $pre.Ddr0Parity]
            set Ddr1Parity [get $pre.Ddr1Parity]
            # don't use ReadEccError as UCE
            # set ReadEccError [get $pre.ReadEccError]
            if {$Ddr0Parity == 1 || $Ddr1Parity == 1 || $ReadEccError == 1} {
                debug "error found in NERR of HA${ha}JCK${jck}"
                set ErrFound 1
                break
            }
        }
    }
    
    if {$ErrFound != 1} {
        error "No error found in HA${ha}"
    }

    if {$Ddr0Parity == 1 || $Ddr1Parity == 1} {
        if {$Ddr0Parity == 1} {
            set DdrCh 0
        } else {
            set DdrCh 1
        } 
        debug "DdrParity found on HA${ha}JCK${jck}DDR${DdrCh}"
        set pre     Ha${ha}Jck${jck}LogDdrParity
        set Dimm0Parity     [get $pre.Ddr${DdrCh}Dimm0Parity]
        set Dimm1Parity     [get $pre.Ddr${DdrCh}Dimm1Parity]
        set Dimm2Parity     [get $pre.Ddr${DdrCh}Dimm2Parity]
        set ch [expr $jck * 2 + $DdrCh]

        set Ddr4Mode  [get [subst MCMTRiMC$ha.ddr4]]
			
        if {$Ddr4Mode == 1} {
		    # cannot isolate DIMM#. only CPU# and CH#.
		    set dimm 255
		    set rank 0
		    debug "LOGDDRPARITY -- (DDR4)Parity error found on ch$ch" 
	    } else {
		    if {$Dimm0Parity == 1 } {
			    set dimm 0
			    set rank 0
			    debug "LOGDDRPARITY -- Parity error found on ch$ch DIMM0" 
		    } elseif {$Dimm1Parity == 1 } {
			    set dimm 1
                set rank 0
                debug "LOGDDRPARITY -- Parity error found on ch$ch DIMM1"
            } else {
			    set dimm 2
			    set rank 0
			    debug "LOGDDRPARITY -- Parity error found on ch$ch DIMM2"
		    }
	    }
    } elseif {$ReadEccError == 1} {
        debug "ReadEccError found on HA${ha}JCK${jck}"
        set pre     Ha${ha}Jck${jck}LogREcc
        set DdrCh       [get $pre.Chn]
        set LogicalRank [get $pre.Rank]
        set ch          [expr $jck *2 + $DdrCh]
        
        if {$LogicalRank < 4} {
            set dimm 0
            set rank ${LogicalRank}
            debug "LOGRECC -- ECC error found on ch$ch DIMM0"
        } elseif {$LogicalRank < 8} {
            set dimm 1
            set rank [expr ${LogicalRank} - 4] 
            debug "LOGRECC -- ECC error found on ch$ch DIMM1"
        } else {
            set dimm 2
            set rank [expr ${LogicalRank} - 8] 
            debug "LOGRECC -- ECC error found on ch$ch DIMM2"
        }
    }

    set LsEn    [get [subst MCMTRiMC$ha.LsEn]]
    set iMCMode [get [subst MCMTRiMC$ha.iMCMode]]
    
    # Adjust channel number if lockstep is enabled
    if {$iMCMode == 2} {
        debug "LockStep mode"
        set ls_en 1
    } else {
        set ls_en 0
    }
    return [list Socket [expr $cpu - 1] iMC $ha Channel [expr $ch] Dimm [expr $dimm] PhysRank $rank ls_en $ls_en]
}

#-----------------------------------------------------------------------------------
# FindFaultDIMM --
#       Find the faulty DIMM according record logged in rd_retry_log CSR.
#       This is function which belongs to workaround of issues that forward translation
#       cannot isolate error down to DIMM level
# Arguements:
#       cpu:    CPU to check
#       ha:     HA(Home Agent) to check
#       ch:     Channel to check
#-----------------------------------------------------------------------------------
proc FindFaultDIMM {cpu ha ch} {
    debug "Finding faulty DIMM on socket:$cpu HA:$ha ch:$ch"
    set pre     iMC${ha}Ch${ch}RetryRdErrLog
    set en      [get $pre.En]
    set valid   [get $pre.Valid]
    set uc      [get $pre.UC]
    set dimm    [get $pre.DIMM]
    set rank    [get $pre.Rank] 
    if {$en != 1 || $valid != 1 || $uc != 1} {
        error "No valid info in Rd_Retry_Log"
    } 
    
    debug "RdRetry -- En:$en valid:$valid uc:$uc dimm:$dimm rank:$rank" 
    set PhysRank $rank
    if { [expr $dimm & $::BIT0] } {
        set DimmNumber 0
    } elseif {[expr $dimm & $::BIT1] } {
        set DimmNumber 1
    } elseif {[expr $dimm & $::BIT2] } {
        set DimmNumber 2
        set PhysRank [expr $PhysRank + 2]
    }

    set LsEn    [get [subst MCMTRiMC$ha.LsEn]]
    set iMCMode [get [subst MCMTRiMC$ha.iMCMode]]
    
    # Adjust channel number if lockstep is enabled
    if {$iMCMode == 2} {
        debug "LockStep mode"
        set ls_en 1
    } else {
        set ls_en 0
    }
    return [list Socket [expr $cpu - 1] iMC $ha Channel $ch Dimm $DimmNumber PhysRank [format %X $PhysRank] ls_en $ls_en]
}

proc AssertMemError {errInfo} {
    array set err $errInfo
    set errType $err(Type)
    instantiate FullLog.Prototype memlog {
        Value                   0x00051003
        Instance                {[format 0x%08X $err(PlannerDimmNum)]}
        RawData.Signature       MEM_
        RawData.Size            {[string length $err(rawData)]}
        RawData.Data            $err(rawData)
        AsciiData.Size          {[string length $err(asciiData)]}
        AsciiData.Data          $err(asciiData)
        }
    
    finalizeLog memlog
    set data [serialize memlog]
    set extended_data "$err(rank) $err(subRank) $err(dramMask) $err(bank) $err(row)"
    if {$::PLATFORM eq "ANDROMEDA"} {
        # group number should be 1-based for memory cards
        set err(socket) [expr $err(socket) + 1]
        lappend ::VGPIO [list $::NodeID  $errType "0.5.0.$err(socket).$err(dimmHex) 1" $extended_data $data]
    } else {
        # group number should be 0 since Nantahala has no memory cards
        lappend ::VGPIO [list $::NodeID  $errType "0.5.0.0.$err(dimmHex) 1" $extended_data $data]
    }
}

#-----------------------------------------------------------------------------------
# processIioError --
#       Dealing with iio error report. Create instance of IioErrorReg and fill in 
#       Aux log with the data.
# Arguements:
#       dev:    Device number of iio root port
#       func:   Function number of iio root port
#-----------------------------------------------------------------------------------
proc processIioError {dev func} {
    debug "Error occurs at $dev $func"

    instantiate IioErrorReg.Prototype iioReg

    set temp [serialize iioReg]
    set size [string length $temp]
    set cpu_num [expr $::TARGET_CPU - 1]
    
    set pre SltCapDev${dev}Func${func}
    set slot [get $pre.PhySlotNumber]
    set asciiData [format "\[S.68005\] An error has been detected by the the IIO core logic on CPU %s. The Global Fatal Error Status register contains 0x%X. The Global Non-Fatal Error Status register contains 0x%X. Please check error logs for the presence of additional downstream device error data." $::TARGET_CPU [getRegister GFERRST] [getRegister GNERRST]]
    
    instantiate FullLog.Prototype iiolog {
        Value           0x00068005
        Instance        {[format 0x%08X $slot]}
        AsciiData.Size  {[string length $asciiData]}
        AsciiData.Data  $asciiData
        RawData.Size    {[format 0x%04X $size]}
        RawData.Data    $temp
    }
    
    finalizeLog iiolog
    set data [serialize iiolog]
    
    set PciSlotHex [format %X $slot]
    set extended_data 0
    lappend ::VGPIO [list $::NodeID  iio "2.1.0.0.$PciSlotHex 1" $extended_data $data]
}

proc CheckSMILane {MemoryBuffer} {
    
    set LinkWidth [get [subst VmseLinkWidth${MemoryBuffer}.CurrWidth]]
    set FailCount [get [subst VmseLinkFail${MemoryBuffer}.Count]]

    if {[expr $LinkWidth & 0x8] == 0 || $FailCount <= 0} {
        return 0
    }
        
    #debug "VMSE CPU $::TARGET_CPU <--> $MemoryBuffer link: Width-$LinkWidth FailCount-$FailCount"

    set asciiData [format "\[S.580B0\] Memory SMI Link Failure Occurred between CPU $::TARGET_CPU and Memory Buffer $MemoryBuffer"]
        instantiate AsciiLog.Prototype smilog {
            Value           0x000580B0
            Instance        {[format %08X [expr $MemoryBuffer]]}
            AsciiData.Size  {[string length $asciiData]}
            AsciiData.Data  $asciiData
        }
    
    finalizeLog smilog
    set data [serialize smilog]
    set extended_data 0
    lappend ::VGPIO [list $::NodeID  mem "0.5.0.0.ff E" $extended_data $data]
    return 1
}


#-----------------------------------------------------------------------------------
# processMemError --
#       Dealing with memory error report. Create instance of memory error detail and fill in 
#       Aux log with the data.
# Arguements:
#       bank:  MCA bank which has the error report
#       address [option]:   Physical address of error logged
#-----------------------------------------------------------------------------------
proc processMemError {bank {addv 0}} {
    debug "Entry $bank [format 0x%016lX $addv]"
    set ls_en 0 ;#lock step enable
    set errType mem
    set status ::MC[subst $bank]_STATUS
    set dimmInfo(Channel) [FromReg 0 4 0 0 [subst $status]]

    set mscod [FromReg 16 16 0 0 [subst $status]]
    set mccod [FromReg 0 16 0 0 [subst $status]]

    if {$dimmInfo(Channel) == 0x0f} {
      debug "MCCOD = [format %04X $mccod]"
      debug "MSCOD = [format %04X $mscod]"
      debug " Error from Cbo or RTID out of range and DIMM location cannot be determined"
      return
    }

    set PprInfo [list   rank 0\
                        subRank 0\
                        dramMask 0\
                        bank 0\
                        row 0]
	
    if { $bank == 7 || $bank == 9 || $bank == 10 || $bank == 11 || $bank == 12} {
        set HA 0 
    } elseif { $bank == 8 || $bank == 13 || $bank == 14 || $bank == 15 || $bank ==16} {
        set HA 1
    }

    set Ddr4Mode [get [subst MCMTRiMC$HA.ddr4]]
    set dimmInfo(iMC) $HA
    if {$bank == 7 || $bank == 8} {
        set errType mem_ha
    }

    if {$mscod == 0x10} {
        set errType mem_ps
    }
    

    debug "Memory UE found on bank-$bank for HA-$HA"

    # Calculate Planner Channel
    # set dimmInfo(PlannerCh) $dimmInfo(Channel)
    # DIMMMap format: DIMMMap($dimmInfo(Socket)-$dimmInfo(PlannerCh)-$dimmInfo(Dimm))
    # EX Socket: 0-3, PlannerCh: 0~7, Dimm: 0-2,
    #   To be reviewed: How about 8s?
    set dimmInfo(PlannerCh) [expr $dimmInfo(iMC) * 4 + $dimmInfo(Channel)]

    # EX: check JCK regs first  
    if {[catch { array set dimmInfo [FindFaultDIMM_JCK  $::TARGET_CPU  $dimmInfo(iMC)]} msg]} {
        debug "FindFaultDIMM_JCK:$msg"
        #check rd_retry_log next    
        if {[catch { array set dimmInfo [FindFaultDIMM  $::TARGET_CPU  $dimmInfo(iMC) $dimmInfo(Channel)]} msg]} {
            debug "FindFaultDIMM:$msg"
        }
	}
	
    #JCK regs or rd_retry_log has valid data. 
    if  {[info exist dimmInfo(Dimm) ] } {
        debug "Found faulty DIMM by JCK regs or rd_retry"
        set dimmInfo(Socket) $::RevCpuMap($dimmInfo(Socket))
        set dimmInfo(VgpioSocket) [expr $dimmInfo(Socket) + 1]
        set dimmInfo(PlannerDimmNum) [findDIMMSilkNum $dimmInfo(Socket) $dimmInfo(PlannerCh) $dimmInfo(Dimm)]
        set rawData [format %c%c%c $dimmInfo(Socket) $dimmInfo(PlannerCh) $dimmInfo(PlannerDimmNum)]
        set dimmInfo(SmiPort) [expr $dimmInfo(PlannerCh)/2]
        set dimmInfo(SmiPortChannel) [expr $dimmInfo(PlannerCh)%2]
        set asciiData(0) [format "\[S.51003\] An uncorrectable memory error was detected in DIMM slot %s on processor %s SMI Port %s Channel %s." $dimmInfo(PlannerDimmNum) $dimmInfo(VgpioSocket) $dimmInfo(SmiPort) $dimmInfo(SmiPortChannel)]
        debug "Socket: $dimmInfo(Socket), PlannerCh: $dimmInfo(PlannerCh), PlannerDimmNum: $dimmInfo(PlannerDimmNum), LockStep:$dimmInfo(ls_en)"
        if { $addv } {
            debug "going to do fwd translation for PPR"
            set reg ::MC${bank}_ADDR
            set addr [FromReg 0 46 0 0 ${reg}]
            debug "Error reported at MC bank $bank with address: [format 0x%016lX $addr]" D_INFO
            if {[catch { array set dimmInfo [forwardXlator $addr]} msg]} {
                debug "forwardXlator:$msg"
            }
            set PprInfo [list   rank $dimmInfo(PhysRank)\
                                subRank $dimmInfo(chip_id)\
                                dramMask 0\
                                bank $dimmInfo(bank)\
                                row $dimmInfo(row)]
            debug "PprInfo = $PprInfo"
        }
    } else {    
        #try to find actual failing DIMM is we have a valid address to translate
        if { $addv } {
            debug "going to do fwd translation"
            set reg ::MC${bank}_ADDR
            set addr [FromReg 0 46 0 0 ${reg}]
            
            debug "Error reported at MC bank $bank with address: [format 0x%016lX $addr]" D_INFO
            if {[catch { array set dimmInfo [forwardXlator $addr]} msg]} {
                debug "forwardXlator:$msg"
            }

            if  {![info exist dimmInfo(Dimm) ] } {
                #Faild to decode the address. Go report error at channel at least. 
                processMemError $bank
                return
            }
            set PprInfo [list   rank $dimmInfo(PhysRank)\
                                subRank $dimmInfo(chip_id)\
                                dramMask 0\
                                bank $dimmInfo(bank)\
                                row $dimmInfo(row)]
            debug "PprInfo = $PprInfo"
            #map back socket from logic num to its phyical num (0-based)
            set dimmInfo(Socket) $::RevCpuMap($dimmInfo(Socket))
            set dimmInfo(VgpioSocket) [expr $dimmInfo(Socket) + 1]
            set dimmInfo(PlannerDimmNum) [findDIMMSilkNum $dimmInfo(Socket) $dimmInfo(PlannerCh) $dimmInfo(Dimm)]
            set rawData [format %c%c%c $dimmInfo(Socket) $dimmInfo(PlannerCh) $dimmInfo(PlannerDimmNum)]
            set dimmInfo(SmiPort) [expr $dimmInfo(PlannerCh)/2]
            set dimmInfo(SmiPortChannel) [expr $dimmInfo(PlannerCh)%2]
            set asciiData(0) [format "\[S.51003\] An uncorrectable memory error was detected in DIMM slot %s on processor %s SMI Port %s Channel %s." $dimmInfo(PlannerDimmNum) $dimmInfo(VgpioSocket) $dimmInfo(SmiPort) $dimmInfo(SmiPortChannel)]
            debug "Socket: $dimmInfo(Socket), PlannerCh: $dimmInfo(PlannerCh), PlannerDimmNum: $dimmInfo(PlannerDimmNum), LockStep:$dimmInfo(ls_en)"
        } else {
            debug "Error reported at MC bank $bank without a valid address"
            debug "Channel found $dimmInfo(PlannerCh)"
            set dimmInfo(Socket) [expr $::TARGET_CPU - 1]
            #map back socket from logic num to its phyical num (0-based)
            set dimmInfo(Socket) $::RevCpuMap($dimmInfo(Socket))
            set dimmInfo(VgpioSocket) [expr $dimmInfo(Socket) + 1]
            set dimmInfo(PlannerDimmNum) 255
            #Need to give more precise iMC and VMSE data here
            set rawData   [format %c%c%c $dimmInfo(Socket) $dimmInfo(PlannerCh) $dimmInfo(PlannerDimmNum)]
            set dimmInfo(SmiPort) [expr $dimmInfo(PlannerCh)/2]
            set dimmInfo(SmiPortChannel) [expr $dimmInfo(PlannerCh)%2]
            set asciiData(0) [format "\[S.51003\] An uncorrectable memory error was detected on processor %s channel %s. The failing DIMM within the channel could not be determined." $dimmInfo(VgpioSocket) $dimmInfo(PlannerCh)]
        }
    }

    # Check SMI lane error for HA
    # loop Memory Buffer 0~1 or 2~3
    set SmiFail 0
    if {$dimmInfo(Channel) == 0xF} {
        for {set MbIndex 0}  {$MbIndex < $::MB_NUMBER} {incr MbIndex} {
            set SmiFail [CheckSMILane $i]
            if {$SmiFail == 1} {
                break;
            }
        }
    } else {
        set TargetSmiLink [expr $dimmInfo(iMC) * 2 + $dimmInfo(Channel) / 2]
        set SmiFail [CheckSMILane $TargetSmiLink]
    }
    if {$SmiFail == 1} {
        #If SMI lane has fail, return SMI failure only.
        debug "SMI lane has failed"
        return
    }

    set errors(0) [list socket $dimmInfo(Socket) PlannerDimmNum $dimmInfo(PlannerDimmNum) dimmHex [format %X $dimmInfo(PlannerDimmNum)] rawData $rawData asciiData $asciiData(0) Type $errType]

    if {[info exist PprInfo]} {
        set errors(0) [concat $errors(0) $PprInfo]
    }

    AssertMemError $errors(0)


    if {$dimmInfo(PlannerDimmNum) != 255 && $dimmInfo(ls_en) == 1 } {
        # report 2 memory error if LockStep is enabled
        set ls_en 1
        findLSPair dimmInfo
        set rawData   [format %c%c%c $dimmInfo(Socket) $dimmInfo(LsPlannerCh) $dimmInfo(LsPlannerDimmNum]
        set asciiData(1) [format "\[S.51003\] An uncorrectable memory error was detected in DIMM slot %s on processor %s SMI Port %s Channel %s." $dimmInfo(LsPlannerDimmNum) $dimmInfo(VgpioSocket) $dimmInfo(LsSmiPort) $dimmInfo(LsSmiPortChannel)]
        set errors(1) [list socket $dimmInfo(Socket) PlannerDimmNum $dimmInfo(LsPlannerDimmNum) dimmHex [format %X $dimmInfo(LsPlannerDimmNum)] rawData $rawData asciiData $asciiData(1) Type $errType]
        if {[info exist PprInfo ]} {
            set errors(1) [concat $errors(1) $PprInfo]
        }
        debug "System in lockstep mode. Decoded Dimm is $dimmInfo(PlannerDimmNum) and its ls pair is $dimmInfo(LsPlannerDimmNum)"
        AssertMemError $errors(1)
    }
}

proc processMemError_MB {HA} {
    set ls_en 0	;#lock step enable
    set errType mem_mb
    set Ddr4Mode  [get [subst MCMTRiMC$HA.ddr4]]

    debug "Check memory error for HA-$HA by JCK"

    array set PprInfo [list rank 0\
                        subRank 0\
                        dramMask 0\
                        bank 0\
                        row 0]

    #check JCK regs first	
    if {[catch { array set dimmInfo [FindFaultDIMM_JCK  $::TARGET_CPU  $HA]} msg]} {
        debug "FindFaultDIMM_JCK:$msg"
    }
    #JCK regs or rd_retry_log has valid data. 
    if  {[info exist dimmInfo(Dimm) ] } {
        debug "dimmInfo = [array get dimmInfo]"	
        set PprInfo(rank) $dimmInfo(PhysRank)
        debug "PprInfo = [array get PprInfo]"
        debug "Found faulty DIMM by JCK regs"
        set dimmInfo(Socket) $::RevCpuMap($dimmInfo(Socket))
        set dimmInfo(VgpioSocket) [expr $dimmInfo(Socket) + 1]
        set dimmInfo(PlannerCh) [expr $dimmInfo(iMC) * 4 + $dimmInfo(Channel)]
        set dimmInfo(PlannerDimmNum) [findDIMMSilkNum $dimmInfo(Socket) $dimmInfo(PlannerCh) $dimmInfo(Dimm)]
        set dimmInfo(SmiPort) [expr $dimmInfo(PlannerCh)/2]
        set dimmInfo(SmiPortChannel) [expr $dimmInfo(PlannerCh)%2]
        set rawData [format %c%c%c $dimmInfo(Socket) $dimmInfo(PlannerCh) $dimmInfo(PlannerDimmNum)]
        set asciiData(0) [format "\[S.51003\] An uncorrectable memory error was detected in DIMM slot %s on processor %s SMI Port %s Channel %s." $dimmInfo(PlannerDimmNum) $dimmInfo(VgpioSocket) $dimmInfo(SmiPort) $dimmInfo(SmiPortChannel)]
        debug "Socket: $dimmInfo(Socket), PlannerCh: $dimmInfo(PlannerCh), dimmNum: $dimmInfo(PlannerDimmNum), LockStep:$dimmInfo(ls_en)"
    } else {
        return
    }
    set errors(0) [list socket $dimmInfo(Socket) PlannerDimmNum $dimmInfo(PlannerDimmNum) dimmHex [format %X $dimmInfo(PlannerDimmNum)] rawData $rawData asciiData $asciiData(0) Type $errType]
    if {[info exist PprInfo ]} {
        set errors(0) [concat $errors(0) [array get PprInfo]]
    }
    AssertMemError $errors(0)

    if {$dimmInfo(PlannerDimmNum) != 255 && $dimmInfo(ls_en) == 1 } {
        set ls_en 1
        findLSPair dimmInfo
        set rawData   [format %c%c%c $dimmInfo(Socket) $dimmInfo(LsPlannerCh) $dimmInfo(LsPlannerDimmNum)]
        set asciiData(1) [format "\[S.51003\] An uncorrectable memory error was detected in DIMM slot %s on processor %s SMI Port %s Channel %s." $dimmInfo(LsPlannerDimmNum) $dimmInfo(VgpioSocket) $dimmInfo(LsSmiPort) $dimmInfo(LsSmiPortChannel)]
        set errors(1) [list socket $dimmInfo(Socket) PlannerDimmNum $dimmInfo(LsPlannerDimmNum) dimmHex [format %X $dimmInfo(LsPlannerDimmNum)] rawData $rawData asciiData $asciiData(1) Type $errType]			
        if {[info exist PprInfo ]} {
            set errors(1) [concat $errors(1) [array get PprInfo]]
        }
        debug "System in lockstep mode. Decoded Dimm is $dimmInfo(PlannerDimmNum) and its ls pair is $dimmInfo(LsPlannerDimmNum)"
        AssertMemError $errors(1)
    }
}

#--------------------------------------------------------------------------------
# processQprError --
#       Dealing with QPI error report. Check if which QPI lane has failed.
# Arguements:
#       bank:  MCA bank which has the error report
#--------------------------------------------------------------------------------
proc reportQpiPoisoning {bank} {
	debug "QPI error found at QPI bank $bank"
	global QPIPOISON_ENTRY
	append ::QPIpoison_log "\nEntry: [format %03d $::QPIPOISON_ENTRY] "
	append ::QPIpoison_log "Cpu:[format %02d [expr $::TARGET_CPU&0x0F]] "
	append ::QPIpoison_log "Core:[format %02d [expr ($::TARGET_CPU&0xF0) >> 4]] "
	incr QPIPOISON_ENTRY
	
	set PoisonEnable [getRegisterEx {Reg MCG_CONTAIN Range {0 1}}]
	#debug "POISON_ENABLE = $PoisonEnable"

	set VAL [FromReg 63 1 0 0 [subst MC[subst $bank]_STATUS]]
	#debug "MC[subst $bank]_STATUS.VAL = $VAL"
	
	if {$PoisonEnable == 0} {
		append ::QPIpoison_log " QPI Poisoning Disabled."
	} elseif {$VAL == 0} {
		append ::QPIpoison_log "NOT QPI Poisoning."
	} else {
		set MISCV [FromReg 59 1 0 0 [subst MC[subst $bank]_STATUS]]
		set ADDRV [FromReg 58 1 0 0 [subst MC[subst $bank]_STATUS]]
		#debug "MC[subst $bank]_STATUS.MISCV = $MISCV"
		#debug "MC[subst $bank]_STATUS.ADDRV = $ADDRV"
	
		if {$ADDRV == 1} {
			set ADDR [FromReg 0 46 0 0 [subst MC[subst $bank]_ADDR]]
		} else {
			set ADDR 0
		}
		#debug "MC[subst $bank]_ADDR.ADDR = [format 0x%0X $ADDR]"

		if {$MISCV == 1} {
			set LSB [FromReg 0 5 0 0 [subst MC[subst $bank]_MISC]]
		} else {
			set LSB 0
		}
		#debug "MC[subst $bank]_MISC.LSB = [format 0x%0X $LSB]"
	
		set Mask [expr ~((1 << ($LSB-1)) - 1)]
		#debug "Mask = [long2hex $Mask]"
		set PhysAddr [expr $ADDR&$Mask]

        if {$::CPUTYPE eq "CPU_IVT"} {
            if {$bank == 4} {
                set Consumer "Intel QPI 2"
            } else {
                set Consumer "Intel QPI 0/1"
            }
        } elseif {$::CPUTYPE eq "CPU_HSX"} {
            if {$bank == $::MCA_BANK_QPI_PORT0_NUMBER} {
                set Consumer "Intel QPI 0"
            } elseif {$bank == $::MCA_BANK_QPI_PORT1_NUMBER} {
                set Consumer "Intel QPI 1"
            } else {
                set Consumer "Intel QPI 2"
            }
        }
        append ::QPIpoison_log "Cunsumer:$Consumer "
        append ::QPIpoison_log "\tCorrupted data: [long2hex $PhysAddr]"
    }
    return
}

#--------------------------------------------------------------------------------
# processQpiMCPoisoning --
#       Dealing with QPI error report. Check if which QPI lane has failed.
# Arguements:
#       port:  MCA bank which has the error report
#--------------------------------------------------------------------------------
proc reportQpiMCPoisoning {port} {
    debug "QPI error found at QPI port $port"
    global QPIPOISON_ENTRY
    append ::QPIpoison_log "Entry: $::QPIPOISON_ENTRY ====\n"
    incr QPIPOISON_ENTRY
    
    set PoisonEnable [getRegisterEx {Reg MCG_CONTAIN Range {0 1}}]
    #debug "POISON_ENABLE = $PoisonEnable"
    
    if {$PoisonEnable == 0} {
        append ::QPIpoison_log "Cpu:            [expr $::TARGET_CPU&0x0F]\n"
        append ::QPIpoison_log "Core:           [expr ($::TARGET_CPU&0xF0) >> 4]\n"
        append ::QPIpoison_log "   QPI Poisoning Disabled.\n"
        return
    }
    
    set VAL [FromReg 63 1 0 0 [subst QPIMC[subst $port]_STATUS]]
    #debug "QPIMC[subst $port]_STATUS.VAL = $VAL"
    
    if {$VAL == 0} {
        append ::QPIpoison_log "Cpu:            [expr $::TARGET_CPU&0x0F]\n"
        append ::QPIpoison_log "Core:           [expr ($::TARGET_CPU&0xF0) >> 4]\n"
        append ::QPIpoison_log "   NOT QPI Poisoning\n"
        return
    }
    
    set MISCV [FromReg 59 1 0 0 [subst QPIMC[subst $port]_STATUS]]
    set ADDRV [FromReg 58 1 0 0 [subst QPIMC[subst $port]_STATUS]]
    #debug "QPIMC[subst $port]_STATUS.MISCV = $MISCV"
    #debug "QPIMC[subst $port]_STATUS.ADDRV = $ADDRV"
    
    if {$ADDRV == 1} {
        set ADDR [FromReg 0 46 0 0 [subst QPIMC[subst $port]_ADDR]]
    } else {
        set ADDR 0
    }
    #debug "QPIMC[subst $port]_ADDR.ADDR = [long2hex $ADDR]"

    if {$MISCV == 1} {
        set LSB [FromReg 0 5 0 0 [subst QPIMC[subst $port]_MISC]]
    } else {
        set LSB 0
    }
    #debug "QPIMC[subst $port]_MISC.LSB = [format 0x%0X $LSB]"
    
    set Mask [expr ~((1 << ($LSB-1)) - 1)]
    #debug "Mask = [long2hex $Mask]"
    set PhysAddr [expr $ADDR&$Mask]
    
    if {$port == 0} {
        set Consumer "QPI 0"
    } elseif {$port == 1} {
        set Consumer "QPI 1"
    } else {
        set Consumer "QPI 2"
    }
    
    append ::QPIpoison_log "Cpu:            [expr $::TARGET_CPU&0x0F]\n"
    append ::QPIpoison_log "Core:           [expr ($::TARGET_CPU&0xF0) >> 4]\n"
    append ::QPIpoison_log "Cunsumer:       $Consumer\n"
    append ::QPIpoison_log "Corrupted data: [long2hex $PhysAddr]\n"
}

set ::QpiErrorAsciiData(0x680B2) "QPI Link Width Reduction Detected"
set ::QpiErrorAsciiData(0x680B8) "QPI Link Failure Detected"
set ::QpiErrorAsciiData(0x680BA) "QPI Link Clock Failover Half Speed Occurred"

proc GetQpiErrorLocationAscii {CpuIndex Port} {
    # CPU# is in one-based to match with label on system, and PORT# is in zero-based to match with Intel document
    set LocationAscii [format "CPU %s QPI port %s." $CpuIndex $Port]
    debug $LocationAscii
    return $LocationAscii
}

proc GetQpiErrorAsciiData {Value CpuIndex Port} {
    set ErrorValue [format 0x%5X [expr $Value & ~1]]
    if {[expr $Value &1] != 0} {
        set Location "External"
    } else {
        set Location "Internal"
    }
    set AsciiData [format "\[W.%05X\] %s %s on %s" $Value $Location $::QpiErrorAsciiData($ErrorValue) [GetQpiErrorLocationAscii $CpuIndex $Port]]
    debug $AsciiData
    return $AsciiData
}

#-----------------------------------------------------------------------------------
# reportQPIFaildown --
#       Dealing with QPI faildown error report(). Create instance of qpi error detail and fill in 
#       Aux log with the data.
# Arguements:
#       port:  MCA bank which has the error report
#-----------------------------------------------------------------------------------
proc reportQPIFaildown {port} {
    # debug "QPI Faildown check start."
    # Port number is reported in external link error
    # Convert Port number to be 1-base in one place for instance and VGPIO field
    set QpiPortNumber [expr $port+1]
    set extended_data 0
    
    # QPI Link, Tx/Rx faildown check
    set TxRegs [getRegister QPIREUT_PH_TDS($port)]
    set RxRegs [getRegister QPIREUT_PH_RDS($port)]
    set TxMaskedRegs [expr $TxRegs & 0x000FFFFF]
    set RxMaskedRegs [expr $RxRegs & 0x000FFFFF]
    if {($TxMaskedRegs != 0x000fffff && $TxRegs != 0xffffffff) || ($RxMaskedRegs != 0x000fffff && $RxRegs != 0xffffffff)} {
        if {$port < 2 && $::PLATFORM eq "NANTAHALA"} {
            #set asciiData [format "\[W.680B3\] External QPI Link Width Reduction Detected"]
            set SelValue 0x680B3
            set SelInstance [format %08X $QpiPortNumber]
            set VgpioInstance $QpiPortNumber
            set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
            set VgpioEventId 9
        } else {
            #set asciiData [format "\[W.680B2\] Internal QPI Link Width Reduction Detected"]
            set SelValue 0x680B2
            set SelInstance [format %08X $::VgpioCpuIndex]
	        set VgpioInstance $::VgpioCpuIndex
            set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
            set VgpioEventId A
        }

        instantiate AsciiLog.Prototype qpifaildownlog {
            Value			$SelValue
            Instance		{$SelInstance}
            AsciiData.Size	{[string length $SelAsciiData]}
            AsciiData.Data	$SelAsciiData
        }

        finalizeLog qpifaildownlog
        set data [serialize qpifaildownlog]

        lappend ::VGPIO [list $::NodeID  qpi "0.1.0.0.$VgpioInstance $VgpioEventId" $extended_data $data]
    }
}

#-----------------------------------------------------------------------------------
# reportQPIFailover --
#       Dealing with QPI clock failover error report(). Create instance of qpi error detail and fill in 
#       Aux log with the data.
# Arguements:
#       port:  MCA bank which has the error report
#-----------------------------------------------------------------------------------
proc reportQPIFailover {port} {
    # debug "QPI clock Failover check start."
    # Port number is reported in external link error
    # Convert Port number to be 1-base in one place for instance and VGPIO field
    set QpiPortNumber [expr $port+1]
    set extended_data 0
    
    # QPIREUT_PH_PIS(8/9/24:3:140) [3:3]: clkfailhalfspdocurd 
    #  1b: Clock failover half speed occurred
    #  0b: default
    # QPI Link 0, Clock failover check
    set Regs [getRegisterEx [subst {Reg QPIREUT_PH_PIS($port) Range {3 1}}]]
    if {$Regs != 0x0} {
        if {$port < 2 && $::PLATFORM eq "NANTAHALA"} {
            #set asciiData [format "\[W.680BB\] External QPI Link Clock Failover Half Speed Occurred"]
            set SelValue 0x680BB
            set SelInstance [format %08X $QpiPortNumber]
            set VgpioInstance $QpiPortNumber
            set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
            set VgpioEventId F
        } else {
            #set asciiData [format "\[W.680BA\] Internal QPI Link Clock Failover Half Speed Occurred"]
            set SelValue 0x680BA
            set SelInstance [format %08X $::VgpioCpuIndex]
	        set VgpioInstance $::VgpioCpuIndex
            set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
            set VgpioEventId E
        }
        instantiate AsciiLog.Prototype qpifailoverlog {
            Value			$SelValue
            Instance		{$SelInstance}
            AsciiData.Size	{[string length $SelAsciiData]}
            AsciiData.Data	$SelAsciiData
        }
        finalizeLog qpifailoverlog
        set data [serialize qpifailoverlog]

        lappend ::VGPIO [list $::NodeID  qpi "0.1.0.0.$VgpioInstance $VgpioEventId" $extended_data $data]		
    }
}

#-----------------------------------------------------------------------------------
# reportQPIPFA --
#       Dealing with QPI PFA error report(). Create instance of qpi error detail and fill in 
#       Aux log with the data.
# Arguements:
#       port:  MCA bank which has the error report
#-----------------------------------------------------------------------------------
proc reportQPIPFA {port} {
    # debug "QPI PFA check start."
    # Port number is reported in external link error
    # Convert Port number to be 1-base in one place for instance and VGPIO field
    set QpiPortNumber [expr $port+1]
    set extended_data 0
    
    set ErrCnt1 [getRegisterEx [subst {Reg QPIERRCNT(0.$port) Range {0 15}}]]
    set ErrCnt2 [getRegisterEx [subst {Reg QPIERRCNT(1.$port) Range {0 15}}]]
    
    if {$ErrCnt1 == 0 && $ErrCnt2 == 0} {
        return
    }
    
    set LinkStatus [getRegisterEx [subst {Reg QPILS($port) Range {24 4}}]]
    
    if {$LinkStatus == 0} {
        return
    }
    
    if {$port < 2 && $::PLATFORM eq "NANTAHALA"} {
        #set asciiData [format "\[W.680B9\] External QPI Link Failure Detected"]
        set SelValue 0x680B9
        set SelInstance [format %08X $QpiPortNumber]
        set VgpioInstance $QpiPortNumber
        set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
        set VgpioEventId 12
    } else {
        #set asciiData [format "\[W.680B8\] Internal QPI Link Failure Detected"]
        set SelValue 0x680B8
        set SelInstance [format %08X $::VgpioCpuIndex]
        set VgpioInstance $::VgpioCpuIndex
        set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
        set VgpioEventId 13
    }
    
    instantiate AsciiLog.Prototype qpiPFAlog {
        Value			$SelValue
        Instance		{$SelInstance}
        AsciiData.Size	{[string length $SelAsciiData]}
        AsciiData.Data	$SelAsciiData
    }

    finalizeLog qpiPFAlog
    set data [serialize qpiPFAlog]

    lappend ::VGPIO [list $::NodeID  qpi "0.1.0.0.$VgpioInstance $VgpioEventId" $extended_data $data]
}

#-----------------------------------------------------------------------------------
# reportQPIMCA --
# 		Dealing with QPI MCA error report(). Create instance of qpi error detail and fill in 
#		Aux log with the data.
# Arguements:
#		bank:  MCA bank which has the error report
#-----------------------------------------------------------------------------------
proc reportQPIMCA {bank} {
	
	set status ::MC[subst $bank]_STATUS
    set mscod [FromReg 16 16 0 0 [subst $status]]
    set extended_data 0

	if {$mscod == 0} {
		return
	}
	
	if {$bank == $::MCA_BANK_QPI_PORT0_NUMBER} {
		set port 0 
	} elseif {$bank == $::MCA_BANK_QPI_PORT1_NUMBER} {
		set port 1 
	} else {
		set port 2
	}

    # Port number is reported in external link error
    # Convert Port number to be 1-base in one place for instance and VGPIO field
    set QpiPortNumber [expr $port+1]

  if {$port < 2 && $::PLATFORM eq "NANTAHALA"} {
        #set asciiData [format "\[W.680B9\] External QPI Link Failure Detected"]
        set SelValue 0x680B9
        set SelInstance [format %08X $QpiPortNumber]
        set VgpioInstance $QpiPortNumber
        set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
        set VgpioEventId 12
    } else {
        #set asciiData [format "\[W.680B8\] Internal QPI Link Failure Detected"]
        set SelValue 0x680B8
        set SelInstance [format %08X $::VgpioCpuIndex]
        set VgpioInstance $::VgpioCpuIndex
        set SelAsciiData [GetQpiErrorAsciiData $SelValue $::VgpioCpuIndex $port]
        set VgpioEventId 13
    }
    
    instantiate AsciiLog.Prototype qpiPFAlog {
        Value			$SelValue
        Instance		{$SelInstance}
        AsciiData.Size	{[string length $SelAsciiData]}
        AsciiData.Data	$SelAsciiData
    }

    finalizeLog qpiPFAlog
    set data [serialize qpiPFAlog]

    lappend ::VGPIO [list $::NodeID  qpi "0.1.0.0.$VgpioInstance $VgpioEventId" $extended_data $data]
}

#--------------------------------------------------------------------------------
# processQpiMCPoisoning --
#       Dealing with QPI error report. Check if which QPI lane has failed.
# Arguements:
#       bank:  MCA bank which has the error report
#--------------------------------------------------------------------------------
proc processQpiError {bank} {
    debug "QPI error found at QPI bank $bank"
    
    reportQpiPoisoning $bank
    
    if {$::CPUTYPE eq "CPU_IVT"} {
        if {$bank == 4} {
		    set port 2
		    reportQPIFaildown $port
#		    reportQPIFailover $port
		    reportQPIPFA $port
	    } else {
		    for { set port 0 } { $port < 2 } { incr port } {
			    reportQPIFaildown $port
#			    reportQPIFailover $port
			    reportQPIPFA $port
		    }
	    }
    } elseif {$::CPUTYPE eq "CPU_HSX"} {
        if {$bank == $::MCA_BANK_QPI_PORT0_NUMBER} {
		    set port 0
		    reportQPIFaildown $port
#		    reportQPIFailover $port
		    reportQPIPFA $port
		    reportQPIMCA $bank
	    } elseif {$bank == $::MCA_BANK_QPI_PORT1_NUMBER} {
		    set port 1
		    reportQPIFaildown $port
#		    reportQPIFailover $port
		    reportQPIPFA $port
		    reportQPIMCA $bank
		} else {
		    set port 2
		    reportQPIFaildown $port
#		    reportQPIFailover $port
		    reportQPIPFA $port
		    reportQPIMCA $bank
		}
	}
}

#--------------------------------------------------------------------------------
# processQpiMCAError --
# 		Dealing with QPI error report. Check if which QPI lane has failed.
# Arguements:
#		bank:  MCA bank which has the error report
#--------------------------------------------------------------------------------
proc processQpiMCAError {bank} {
	debug "QPI error found at MCA bank $bank"
	
    reportQPIMCA $bank
}

#--------------------------------------------------------------------------------
# processQprMCError --
#       Dealing with QPI error report. Check if which QPI lane has failed.
# Arguements:
#       bank:  MCA bank which has the error report
#--------------------------------------------------------------------------------
proc processQpiMCError {port} {
    debug "QPI error found at QPI port $port"
    
    reportQpiMCPoisoning $port
    reportQPIFaildown $port
#   reportQPIFailover $port
    reportQPIPFA $port
}

proc reportCpuError {cpu type} {
    if {[info exist ::CpuErrorList($cpu-$type)]} {
        return
    }
    set ::CpuErrorList($cpu-$type) 1

    if {$type eq "IERR"} {
        set asciiData "Processor CATERR(IERR) has asserted"
        set value 0x0001100B
        set event b
    } elseif {$type eq "MCERR"} {
        set asciiData "Uncorrectable processor error detected"
        set value 0x0001100C
        set event 8
    }
        
    instantiate AsciiLog.Prototype cpulog {
        Value                   $value
        Instance                {[format %08X $cpu]}
        AsciiData.Size          {[string length $asciiData]}
        AsciiData.Data          $asciiData
    }
    
    finalizeLog cpulog
    set data [serialize cpulog]
    set extended_data 0
    lappend ::VGPIO [list $::NodeID cpu "0.1.0.0.$cpu $event" $extended_data $data]
}

#--------------------------------------------------------------------------------
# processCpuError --
#       Dealing with cpu error report. Check if the cpu error is MCERR or IERR.
#       Also detects the error source packge according to MCA_SRC_LOG register
# Arguements:
#       bank:  MCA bank which has the error report
#--------------------------------------------------------------------------------
proc processCpuError {bank} {
    set currP $::TARGET_CPU 
    set status ::MC[subst $bank]_STATUS
    set status [FromReg 0 64 0 0 [subst $status]]
    debug "Process CPU error on bank:$bank status: [long2hex $status]"
    if {$::IERR_DETECTED} {
        debug "IERR was externally defined. Report IERR" D_VERBOSE
        set vgpio [list $::NodeID  cpu subst ["0.1.0.0.$::TARGET_CPU B"] {}]
    } elseif { $::MAX_CPU == 1 } {
        # Currently we cannot not distinquish MCERR/IERR in one CPU condition.
        # Assert general (MCERR) in one CPU case
        debug "Only one cpu is detect. We can't tell is the error is MCERR or IERR"
        reportCpuError $::TARGET_CPU MCERR
    } else {	    
      set src [getRegister MCA_ERR_SRC_LOG]
      set instance $currP
      if { [expr $src&$::BIT28]} {
          debug "CATERR Found."           
          if { [expr $src&$::BIT27]} {
              #IERR detected. 
              debug "IERR Found."
              set errType IERR
          } elseif { [expr $src&$::BIT26]} {
              #MCERR detected. 
              debug "MCERR Found."
              set errType MCERR
          }
      } 
      if { [expr $src&$::BIT20]} {
          debug "MSMI Found."         
          if { [expr $src&$::BIT19]} {
              #IERR detected. 
              debug "MSMI IERR Found."
              set errType IERR
          } elseif { [expr $src&$::BIT18]} {
              #MCERR detected. 
              debug "MSMI MCERR Found."
              set errType MCERR
          }
      }
    }
    if { [info exist errType] } {   
        debug "report CPU VGPIO $errType"
        reportCpuError $instance $errType
    } else {
        debug "no error found in MCA_ERR_SRC_LOG"
    }
}

#--------------------------------------------------------------------------------
# initializePciAddress --
#   Initialize all PciAddress
#--------------------------------------------------------------------------------
proc initializePciAddress {} {
    set ::QPI0_DEV      8
    set ::QPI1_DEV      9
    set ::IIO_DEV       5
    set ::VMSE0_DEV    17
    set ::VMSE1_DEV    31

	if {$::CPUTYPE eq "CPU_IVT"} {
        set ::QPI2_DEV     24
        set ::PCU_DEV      10
        set ::CBO_DEV      22
		set ::CBO_SAD_DRAM_FUNC  0
		set ::CBO_SAD_IO_FUNC    1
        set ::HA0_DEV      14
        set ::HA1_DEV      28
        set ::HA0_FUNC     0
        set ::HA1_FUNC     0
        set ::IMC0_DEV     15
        set ::IMC1_DEV     29
        set ::IMC0_CHANNEL 16
        set ::IMC1_CHANNEL 30  

        set ::IMC_MAIN_0_FUNC    0
        set ::IMC_MAIN_1_FUNC    1

        set ::IMC0_CH01_DEV 16
        set ::IMC0_CH23_DEV 16
        set ::IMC1_CH01_DEV 30
        set ::IMC1_CH23_DEV 30

        set ::IMC_DESC0_FUNC 6
        set ::IMC_DESC1_FUNC 7
        set ::IMC_DESC2_FUNC 2
        set ::IMC_DESC3_FUNC 3

        set ::UBOX_DEV           11
        set ::UBOX_SCRATCH_FUNC  0

        set ::MCA_BANK_QPI_PORT0_NUMBER 5
    } elseif {$::CPUTYPE eq "CPU_HSX"} {
        set ::QPI2_DEV     10
        set ::PCU_DEV      30
        set ::CBO_DEV      15
        set ::CBO_SAD_DRAM_FUNC  4
        set ::CBO_SAD_IO_FUNC    5
        set ::HA0_DEV      18
        set ::HA1_DEV      18
        set ::HA0_FUNC     0
        set ::HA1_FUNC     4
        set ::IMC0_DEV     19
        set ::IMC1_DEV     22
        set ::IMC0_CHANNEL 20
        set ::IMC1_CHANNEL 23

        set ::IMC_MAIN_0_FUNC    0
        set ::IMC_MAIN_1_FUNC    1

        set ::IMC_DESC0_FUNC 2
        set ::IMC_DESC1_FUNC 3
        set ::IMC_DESC2_FUNC     4
        set ::IMC_DESC3_FUNC     5

        set ::IMC0_CH01_DEV 20
        set ::IMC0_CH23_DEV 21
        set ::IMC1_CH01_DEV 23
        set ::IMC1_CH23_DEV 24
        set ::UBOX_DEV           16
        set ::UBOX_SCRATCH_FUNC  5
 
        set ::MCA_BANK_QPI_PORT0_NUMBER 5
        set ::MCA_BANK_QPI_PORT1_NUMBER 20
        set ::MCA_BANK_QPI_PORT2_NUMBER 21
    }

    array set ::QpiDeviceId "0 $::QPI0_DEV 1 $::QPI1_DEV 2 $::QPI2_DEV"

    # Different SKU with different IMC Device/Function number
    # Detail in UeRegisters.h
    set ::IMC_DP_0_FUNC 2   ;# Channel 0, 2
    set ::IMC_DP_1_FUNC 3   ;# Channel 1, 3
    array set ::MC_DEV_CHANNEL_MAP "0 $::IMC0_CH01_DEV 1 $::IMC0_CH01_DEV 2 $::IMC0_CH23_DEV 3 $::IMC0_CH23_DEV 4 $::IMC1_CH01_DEV 5 $::IMC1_CH01_DEV 6 $::IMC1_CH23_DEV 7 $::IMC1_CH23_DEV"
    array set ::MC_FUN_CHANNEL_MAP "0 $::IMC_DP_0_FUNC 1 $::IMC_DP_1_FUNC 2 $::IMC_DP_0_FUNC 3 $::IMC_DP_1_FUNC 4 $::IMC_DP_0_FUNC 5 $::IMC_DP_1_FUNC 6 $::IMC_DP_0_FUNC 7 $::IMC_DP_1_FUNC"    
    if {$::EX_SUPPORT == 0} {
        if {$::HA_PER_SOCKET == 2} {
            set ::IMC_CH01_DEV  $::IMC0_CH01_DEV    ;# 20
            set ::IMC_CH23_DEV  $::IMC1_CH01_DEV    ;# 23
        } else {
            set ::IMC_CH01_DEV  $::IMC0_CH01_DEV    ;# 20
            set ::IMC_CH23_DEV  $::IMC0_CH23_DEV    ;# 21
        }            
        array set ::MC_DEV_CHANNEL_MAP "0 $::IMC_CH01_DEV 1 $::IMC_CH01_DEV 2 $::IMC_CH23_DEV 3 $::IMC_CH23_DEV"
    }
}

#--------------------------------------------------------------------------------
# initialize --
#   Initialize all handlers and global symbols.
#--------------------------------------------------------------------------------
proc initialize {} {
    
    #Clean up old setting.
    array unset ::HANDLER
        
    init_log vgpio_log
    init_log errlog
        
    if {[info exist ::IERR_DETECTED] == 0 } {
        debug "IERR_DETECTED is not provided. Define it here and its value as 0"
        set ::IERR_DETECTED 0
    }
    
    #build up bits map first.
    for { set i 0 } { $i < 64 } { incr i} {
        set ::BIT${i} [long2hex [expr 1 << $i]]
    }
    
    set ::SEV1 $::BIT1
    set ::SEV2 $::BIT2
    
    set ::VAL   $::BIT63
    set ::OVER  $::BIT62
    set ::UC    $::BIT61
    set ::EN    $::BIT60
    set ::MISCV $::BIT59
    set ::ADDRV $::BIT58
    set ::PCC   $::BIT57
    set ::S     $::BIT56
    set ::AR    $::BIT55
    
    #setup csr register address here
    set ::GSYSST                CSR:[mapCSR 0x00 $::IIO_DEV 0x02 0x01CC]
    set ::GNERRST               CSR:[mapCSR 0x00 $::IIO_DEV 0x02 0x01C0]
    set ::GFERRST               CSR:[mapCSR 0x00 $::IIO_DEV 0x02 0x01C4]
    set ::MCA_ERR_SRC_LOG       CSR:[mapCSR 0x01 $::PCU_DEV 0x02 0xEC]
    
    # HASYSDEFEATURE
    define HADefeature    "Address {CSR:[mapCSR 0x01 $::HA0_DEV $::HA0_FUNC 0x80]} MirrorMod {5 2} LockstepEn {7 1}"

    # HASYSDEFEATURE2
    define HADefeature2iMC0     "Address {CSR:[mapCSR 0x01 $::HA0_DEV $::HA0_FUNC 0x84]} MCChanHashEn {21 1} MCChanShiftUpEnable {22 1} PtlMirEn {28 1}"
    define HADefeature2iMC1     "Address {CSR:[mapCSR 0x01 $::HA1_DEV $::HA1_FUNC 0x84]} MCChanHashEn {21 1} MCChanShiftUpEnable {22 1} PtlMirEn {28 1}"

    # mcmtr
    set csr1 CSR:[mapCSR 0x01 $::IMC0_DEV 0x00 0x7C]
    set csr2 CSR:[mapCSR 0x01 $::IMC1_DEV 0x00 0x7C]
    set bitmap "iMCMode {12 2} LsEn {1 1} ClosePG {0 1} ddr4 {14 1} bank_xor_enable {9 1}"
    set Map1 "Address $csr1 $bitmap"
    set Map2 "Address $csr2 $bitmap"
    define MCMTRiMC0      "$Map1"
    define MCMTRiMC1      "$Map2"

    # HaChFailSts
    set ::HACHFAILSTS(0) "CSR:[mapCSR 0x01 $::HA0_DEV $::HA0_FUNC 0x8c]"
    set ::HACHFAILSTS(1) "CSR:[mapCSR 0x01 $::HA1_DEV $::HA1_FUNC 0x8c]"

    #set MCA Global Register address here
    set ::MCG_CONTAIN     MSR:0x0178
    
    #setup MSR register address here
    # IA32_MC0_CTL ~ IA32_MC31_CTL
    for {  set i 0 } { $i < $::MCA_BANK_NUM } { incr i} {
        set offset [expr $i * 4]
        set ::MC[subst $i]_CTL       MSR:[format 0x%04X [expr $::MCA_BASE + $offset]]
        set ::MC[subst $i]_STATUS    MSR:[format 0x%04X [expr $::MCA_BASE + $offset + 1]]
        set ::MC[subst $i]_ADDR      MSR:[format 0x%04X [expr $::MCA_BASE + $offset + 2]]
        set ::MC[subst $i]_MISC      MSR:[format 0x%04X [expr $::MCA_BASE + $offset + 3]]
    }
    
    #setup additional MSR for QPI error 
    for {  set i 0 } { $i < $::MAX_QPI_PORT } { incr i} {
        set offset [expr $i * 4]
        set ::QPIMC[subst $i]_CTL        MSR:[format 0x%04X [expr $::QPI_MCA_BASE + $offset]]
        set ::QPIMC[subst $i]_STATUS     MSR:[format 0x%04X [expr $::QPI_MCA_BASE + $offset + 1]]
        set ::QPIMC[subst $i]_ADDR       MSR:[format 0x%04X [expr $::QPI_MCA_BASE + $offset + 2]]
        set ::QPIMC[subst $i]_MISC       MSR:[format 0x%04X [expr $::QPI_MCA_BASE + $offset + 3]]
    }
    
    #setup CPUNODEID
    set ::CPUNodeId     CSR:[mapCSR 0x01 $::UBOX_DEV $::UBOX_SCRATCH_FUNC 0x40]

    #Set memory address related register address alias
    # MESEG_BASE
    set ::MEBaseRegLow  CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_DRAM_FUNC 0x50]
    set ::MEBaseRegHigh     CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_DRAM_FUNC 0x50]
    set ::MELimitLow    CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_DRAM_FUNC 0x58]
    set ::MELimitHigh   CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_DRAM_FUNC 0x5C]
    # TSEG
    set ::TSegLow       CSR:[mapCSR 0x00 $::IIO_DEV 0x00 0xA8]
    set ::TSegHigh      CSR:[mapCSR 0x00 $::IIO_DEV 0x00 0xAC]
    # MMCFG
    set ::MMCFGLow      CSR:[mapCSR 0x01 $::IIO_DEV 0x00 0x90]
    set ::MMCFGHigh     CSR:[mapCSR 0x01 $::IIO_DEV 0x00 0x98]
    # TOLM
    set ::TOLM      CSR:[mapCSR 0x01 $::IIO_DEV 0x00 0xD0]
    # TOHM
    set ::TOHM      CSR:[mapCSR 0x01 $::IIO_DEV 0x00 0xD4]
    
    
    # MMIO_RULE_[0:15]
    for {set i 0} {$i < $::DECODE_ENTRIES(SAD_MMIO)} {incr i} {
        set ::MMIORuleHigh($i)  CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_IO_FUNC [expr 0x44 + $i*8 ]]
        set ::MMIORuleLow($i)   CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_IO_FUNC [expr 0x40 + $i*8 ]]
    }
    
    # DRAM_RULE, DRAM_RULE_[1:19]
    for {set i 0} {$i < $::DECODE_ENTRIES(SAD_DRAM)} {incr i} {
        set ::DRAMRule($i)  CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_DRAM_FUNC [expr 0x60 + $i*8 ]]
    }
    
    # INTERLEAVE_LIST, INTERLEAVE_LIST_[1:19]
    for {set i 0} {$i < $::DECODE_ENTRIES(SAD_DRAM)} {incr i} {
        set ::InterleaveList($i)    CSR:[mapCSR 0x01 $::CBO_DEV $::CBO_SAD_DRAM_FUNC [expr 0x64 + $i*8 ]]
    }
    
    # TAD[0:11]
    for {set i 0} {$i < $::DECODE_ENTRIES(TAD)} {incr i} {  
        set ::TAD(0-$i) CSR:[mapCSR 0x01 $::HA0_DEV $::HA0_FUNC [expr 0x40 + $i*4]]
        set ::TAD(1-$i) CSR:[mapCSR 0x01 $::HA1_DEV $::HA1_FUNC [expr 0x40 + $i*4]]
    }
    # TADWAYNESS_[0:11]
    for {set i 0} {$i < $::DECODE_ENTRIES(TAD)} {incr i} {  
        set ::TADWayness(0-$i) CSR:[mapCSR 0x01 $::IMC0_DEV $::IMC_MAIN_0_FUNC [expr 0x80 + $i*4]]
        set ::TADWayness(1-$i) CSR:[mapCSR 0x01 $::IMC1_DEV $::IMC_MAIN_0_FUNC [expr 0x80 + $i*4]]
    }
    
    # TADCHNILVOFFSET_[0:11]
    for {set i 0} {$i < $::DECODE_ENTRIES(TAD)} {incr i} {  
        for {set j 0} {$j < $::CHANNEL_PER_IMC} {incr j} {
            set csr1 CSR:[mapCSR 0x01 $::IMC0_DEV [expr $::IMC_DESC0_FUNC + $j] [expr 0x90 + $i * 4]]
            set csr2 CSR:[mapCSR 0x01 $::IMC1_DEV [expr $::IMC_DESC0_FUNC + $j] [expr 0x90 + $i * 4]]
            set endfix Ch${j}TADCHNILVOFFSET${i}
            set bitmap {A6Shift {0 3} TadOffset {6 20} ChIdxOffset {28 2}}
            
            set Map1 "Address $csr1 $bitmap"
            set Map2 "Address $csr2 $bitmap"
            define iMC0$endfix "$Map1"
            define iMC1$endfix "$Map2"
        }
    }
    
    # RIRWAYNESSLIMIT_[0:4]
    for {set i 0} {$i < $::CHANNEL_PER_IMC} {incr i} {
        for {set j 0} {$j < $::DECODE_ENTRIES(RIR)} {incr j} {
            set ::RIR(0-$i-$j) CSR:[mapCSR 0x01 $::IMC0_DEV [expr $i + $::IMC_DESC0_FUNC] [expr $j*4 + 0x108]]
            set ::RIR(1-$i-$j) CSR:[mapCSR 0x01 $::IMC1_DEV [expr $i + $::IMC_DESC0_FUNC] [expr $j*4 + 0x108]]
        }
    }
    
    # RIRILV0OFFSET_[0:4]
    # RIRILV1OFFSET_[0:4]
    # RIRILV2OFFSET_[0:4]
    # RIRILV3OFFSET_[0:4]
    # RIRILV4OFFSET_[0:4]
    # RIRILV5OFFSET_[0:4]
    # RIRILV6OFFSET_[0:4]
    # RIRILV7OFFSET_[0:4]
    for {set i 0} {$i < $::CHANNEL_PER_IMC} {incr i} {
        for {set j 0} {$j < 8} {incr j} {
            for {set k 0} {$k < $::DECODE_ENTRIES(RIR)} {incr k} {
                set csr1 "CSR:[mapCSR 0x01 $::IMC0_DEV [expr $::IMC_DESC0_FUNC + $i] [expr 0x120 + $j * 4 + $k * 0x20]]"
                set csr2 "CSR:[mapCSR 0x01 $::IMC1_DEV [expr $::IMC_DESC0_FUNC + $i] [expr 0x120 + $j * 4 + $k * 0x20]]"
                set endfix Ch${i}RIRILV${j}OFFSET${k}

                set bitmap {RirRankTgt {16 4} RirOffset {2 13}}
                set Map1 "Address $csr1 $bitmap"
                set Map2 "Address $csr2 $bitmap"
                define iMC0$endfix "$Map1"
                define iMC1$endfix "$Map2"
            }
        }
    }
    
    # RETRY_RD_ERR_LOG
    for {set Ha 0} {$Ha < $::HA_PER_SOCKET} {incr Ha} {
        for {set SubChannel 0} {$SubChannel < $::CHANNEL_PER_IMC} {incr SubChannel} {
            set Channel [expr $Ha*$::CHANNEL_PER_IMC+$SubChannel]
            set dev $::MC_DEV_CHANNEL_MAP($Channel)
            set func $::MC_FUN_CHANNEL_MAP($Channel)
            set csr CSR:[mapCSR 0x01 $dev $func 0x154]
            set endfix Ch${SubChannel}RetryRdErrLog
            set bitmap "Valid {0 1} UC {1 1} DIMM {8 3} Rank {11 3} En {31 1}"
            set Map "Address $csr $bitmap"
            define iMC${Ha}$endfix "$Map"
        }
    }

    # amap
    for {set ch 0} {$ch < $::CHANNEL_PER_IMC} {incr ch} {
        set csr1 "CSR:[mapCSR 0x01 $::IMC0_DEV [expr $::IMC_DESC0_FUNC + $ch] 0x8c]"
        set csr2 "CSR:[mapCSR 0x01 $::IMC1_DEV [expr $::IMC_DESC0_FUNC + $ch] 0x8c]"
        set endfix Ch${ch}_amap

        set bitmap "hsxplus {0 1}"
        set Map1 "Address $csr1 $bitmap"
        set Map2 "Address $csr2 $bitmap"
        define iMC0_$endfix "$Map1"
        define iMC1_$endfix "$Map2"
    }

    # dimmmtr_0
    # dimmmtr_1
    # dimmmtr_2
    for {set Ch 0} {$Ch < $::CHANNEL_PER_IMC} {incr Ch} {
        for {set Dimm 0} {$Dimm < 3} {incr Dimm} {
            set csr1 "CSR:[mapCSR 0x01 $::IMC0_DEV [expr $::IMC_DESC0_FUNC + $Ch] [expr 0x80 + $Dimm * 4]]"
            set csr2 "CSR:[mapCSR 0x01 $::IMC1_DEV [expr $::IMC_DESC0_FUNC + $Ch] [expr 0x80 + $Dimm * 4]]"
            set endfix Ch${Ch}_dimmmtr_${Dimm}

            set bitmap "ca_width {0 2} ra_width {2 3}  dimm_pop {14 1} rank_disable {16 4} ddr4_mode {20 1} ddr4_3dsnumranks_cs {23 2}"
            set Map1 "Address $csr1 $bitmap"
            set Map2 "Address $csr2 $bitmap"
            define iMC0_$endfix "$Map1"
            define iMC1_$endfix "$Map2"
        }
    }

    # VMSE_LINK_WIDTH
    for {set MemoryBuffer 0} {$MemoryBuffer < $::MB_NUMBER} {incr MemoryBuffer} {
        set dev $::MC_DEV_CHANNEL_MAP([expr $MemoryBuffer * 2])
        set func $::MC_FUN_CHANNEL_MAP([expr $MemoryBuffer * 2])
        define VmseLinkWidth[subst $MemoryBuffer] "Address {CSR:[mapCSR 0x1 $dev $func 0x41c]} CurrWidth {0 4}"
        define VmseLinkFail[subst $MemoryBuffer] "Address {CSR:[mapCSR 0x1 $dev $func 0x414]} Threshold {8 8} Count {0 8}"
    }

    #Set QPI register address here
    for {set p 0} {$p < 2} {incr p} {
        for {set l 0} {$l < 3} {incr l} {
            set ::QPIERRCNT([subst $p].[subst $l]) CSR:[mapCSR 0x01 $::QpiDeviceId($l) 0x00 [expr 0x78 + $p*4]]
        }
    }
    
    for {set l 0} {$l < 3} {incr l} {
        set ::QPILS([subst $l]) CSR:[mapCSR 0x01 $::QpiDeviceId($l) 0x00 0x48]
    }
    
    for {set l 0} {$l < 3} {incr l} {
        set ::QPIREUT_PH_TDS([subst $l]) CSR:[mapCSR 0x01 $::QpiDeviceId($l) 0x03 0x134]
    }
    
    for {set l 0} {$l < 3} {incr l} {
        set ::QPIREUT_PH_RDS([subst $l]) CSR:[mapCSR 0x01 $::QpiDeviceId($l) 0x03 0x138]
    }
    
    for {set l 0} {$l < 3} {incr l} {
        set ::QPIREUT_PH_PIS([subst $l]) CSR:[mapCSR 0x01 $::QpiDeviceId($l) 0x03 0x140]
    }

    # SLTCAP
    if {$::PLATFORM eq "ANDROMEDA"} {
        set bitmap "PhySlotNumber {19 6}"
    } elseif {$::PLATFORM eq "NANTAHALA"} {
        set bitmap "PhySlotNumber {19 4}"
    }
    define SltCapDev0Func0 "Address {CSR:[mapCSR 0x00 0x00 0x00 0x0A4]} $bitmap"
    define SltCapDev1Func0 "Address {CSR:[mapCSR 0x00 0x01 0x00 0x0A4]} $bitmap"
    define SltCapDev1Func1 "Address {CSR:[mapCSR 0x00 0x01 0x01 0x0A4]} $bitmap"
    define SltCapDev2Func0 "Address {CSR:[mapCSR 0x00 0x02 0x00 0x0A4]} $bitmap"
    define SltCapDev2Func1 "Address {CSR:[mapCSR 0x00 0x02 0x01 0x0A4]} $bitmap"
    define SltCapDev2Func2 "Address {CSR:[mapCSR 0x00 0x02 0x02 0x0A4]} $bitmap"
    define SltCapDev2Func3 "Address {CSR:[mapCSR 0x00 0x02 0x03 0x0A4]} $bitmap"
    define SltCapDev3Func0 "Address {CSR:[mapCSR 0x00 0x03 0x00 0x0A4]} $bitmap"
    define SltCapDev3Func1 "Address {CSR:[mapCSR 0x00 0x03 0x01 0x0A4]} $bitmap"
    define SltCapDev3Func2 "Address {CSR:[mapCSR 0x00 0x03 0x02 0x0A4]} $bitmap"
    define SltCapDev3Func3 "Address {CSR:[mapCSR 0x00 0x03 0x03 0x0A4]} $bitmap"
    # JCK regs
    # FERR
    for {set j 0} {$j < 2} {incr j} {
        set endfix Jck${j}FErr
        set Ha0Jck JCK:$j-0x0200
        set Ha1Jck JCK:[expr ($j + 2)]-0x0200
        set bitmap "VmseWEccErr {2 1} Ddr0Parity {3 1} Ddr1Parity {4 1} ReadEccError {7 1}"
        define Ha0$endfix "Address $Ha0Jck $bitmap"
        define Ha1$endfix "Address $Ha1Jck $bitmap"
    }
    # NERR
    for {set j 0} {$j < 2} {incr j} {
        set endfix Jck${j}NErr
        set Ha0Jck JCK:$j-0x0204
        set Ha1Jck JCK:[expr ($j + 2)]-0x0204
        set bitmap "VmseWEccErr {2 1} Ddr0Parity {3 1} Ddr1Parity {4 1} ReadEccError {7 1}"
        define Ha0$endfix "Address $Ha0Jck $bitmap"
        define Ha1$endfix "Address $Ha1Jck $bitmap"
    }
    # LOGDDRPAIRTY
    for {set j 0} {$j < 2} {incr j} {
        set endfix Jck${j}LogDdrParity
        set Ha0Jck JCK:$j-0x0210
        set Ha1Jck JCK:[expr ($j + 2)]-0x0210
        if {$::PLATFORM eq "ANDROMEDA"} {
            set bitmap "Ddr0Dimm0Parity {0 1} Ddr0Dimm1Parity {1 1} Ddr0Dimm2Parity {2 1} Ddr1Dimm0Parity {18 1} Ddr1Dimm1Parity {17 1} Ddr1Dimm2Parity {16 1}"
        } elseif {$::PLATFORM eq "NANTAHALA"} {
            set bitmap "Ddr0Dimm0Parity {0 1} Ddr0Dimm1Parity {1 1} Ddr0Dimm2Parity {2 1} Ddr1Dimm0Parity {16 1} Ddr1Dimm1Parity {17 1} Ddr1Dimm2Parity {18 1}"
        }
        define Ha0$endfix "Address $Ha0Jck $bitmap"
        define Ha1$endfix "Address $Ha1Jck $bitmap"
    }
    # LOGRECC
    for {set j 0} {$j < 2} {incr j} {
        set endfix Jck${j}LogREcc
        set Ha0Jck JCK:$j-0x0244
        set Ha1Jck JCK:[expr ($j + 2)]-0x0244
        set bitmap "EccMode {0 2} Chn {2 1} Rank {3 4}"
        define Ha0$endfix "Address $Ha0Jck $bitmap"
        define Ha1$endfix "Address $Ha1Jck $bitmap"
    }

    # IerrLoggingReg
    set csr CSR:[mapCSR 0x01 $::UBOX_DEV $::UBOX_SCRATCH_FUNC 0xA4]
    set bitmap "FirstIerrSrcId {0 8} FirstIerrSrcValid {8 1} FirstIerrSrcFromCbo {9 1} SecondIerrSrcId {16 8} SecondIerrSrcValid {24 1} SecondIerrSrcFromCbo {25 1}"
    set Map "Address $csr $bitmap"
    define IerrLoggingReg "$Map"

    # MCerrLoggingReg
    set csr CSR:[mapCSR 0x01 $::UBOX_DEV $::UBOX_SCRATCH_FUNC 0xA8]
    set bitmap "FirstMCerrSrcId {0 8} FirstMCerrSrcValid {8 1} FirstMCerrSrcFromCbo {9 1} SecondMCerrSrcId {16 8} SecondMCerrSrcValid {24 1} SecondMCerrSrcFromCbo {25 1}"
    set Map "Address $csr $bitmap"
    define MCerrLoggingReg "$Map"

    if {$::PLATFORM eq "NANTAHALA"} {
        set ::CpuPerNode 2
    } elseif {$::PLATFORM eq "ANDROMEDA"} {
        set ::CpuPerNode 4
    }


}

#--------------------------------------------------------------------------------
# setupHandler : Setup handlers for each register we need to look into
#
# HANDLER prototype:
#   HANDLER (register_name-test_bit)
#     True case for (register_name-value & test_bit) is true.
#  HANDLER (register_name-!test_bit)
#     Fail case for (register_name-value & test_bit) is false.
#--------------------------------------------------------------------------------
proc setupHandler { } {
    set ::extended_data 0

    #-------------------iio related error handlers----------------------------------------
    set ::HANDLER($::GSYSST) "branch GSYSST {SEV2 SEV1}"
    set ::HANDLER($::GSYSST-$::SEV2) "check GFERRST"
    set ::HANDLER($::GSYSST-$::SEV1) "check GNERRST"
        
    set ::HANDLER($::GFERRST) "branch GFERRST {BIT25 BIT24 BIT23 BIT20 BIT15 BIT14 BIT13 BIT12 BIT11 BIT10 BIT9 BIT8 BIT7 BIT6 BIT5}"
    set ::HANDLER($::GNERRST) "branch GNERRST {BIT25 BIT24 BIT23 BIT20 BIT15 BIT14 BIT13 BIT12 BIT11 BIT10 BIT9 BIT8 BIT7 BIT6 BIT5}"
    
    #VT-d
    set ::HANDLER($::GFERRST-$::BIT25) {lappend ::VGPIO [list $::NodeID  iio "2.1.0.0.0 1" $::extended_data {}] }
    #MISC
    set ::HANDLER($::GFERRST-$::BIT24) {lappend ::VGPIO [list $::NodeID  iio "2.1.0.0.0 1" $::extended_data {}] }
    #IIO Core
    set ::HANDLER($::GFERRST-$::BIT23) {lappend ::VGPIO [list $::NodeID  iio "2.1.0.0.0 1" $::extended_data {}] }
    #DMI2
    set ::HANDLER($::GFERRST-$::BIT20) {lappend ::VGPIO [list $::NodeID  iio "2.1.0.0.0 1" $::extended_data {}] }
    
    #Check fatal error source bits
    #p3d
    set ::HANDLER($::GFERRST-$::BIT15) {processIioError 3 3}
    #p3c
    set ::HANDLER($::GFERRST-$::BIT14) {processIioError 3 2}
    #p3b
    set ::HANDLER($::GFERRST-$::BIT13) {processIioError 3 1}
    #p3a
    set ::HANDLER($::GFERRST-$::BIT12) {processIioError 3 0}
    #p2d
    set ::HANDLER($::GFERRST-$::BIT11) {processIioError 2 3}
    #p2c
    set ::HANDLER($::GFERRST-$::BIT10) {processIioError 2 2}
    #p2b
    set ::HANDLER($::GFERRST-$::BIT9)  {processIioError 2 1}
    #p2a
    set ::HANDLER($::GFERRST-$::BIT8)  {processIioError 2 0}
    #p1b
    set ::HANDLER($::GFERRST-$::BIT7)  {processIioError 1 1}
    #p1a
    set ::HANDLER($::GFERRST-$::BIT6)  {processIioError 1 0}
    #DMI
    set ::HANDLER($::GFERRST-$::BIT5)  {processIioError 0 0}
    
    #Check non-fatal error source bits
    #p3d
    set ::HANDLER($::GNERRST-$::BIT15) {processIioError 3 3}
    #p3c
    set ::HANDLER($::GNERRST-$::BIT14) {processIioError 3 2}
    #p3b
    set ::HANDLER($::GNERRST-$::BIT13) {processIioError 3 1}
    #p3a
    set ::HANDLER($::GNERRST-$::BIT12) {processIioError 3 0}
    #p2d
    set ::HANDLER($::GNERRST-$::BIT11) {processIioError 2 3}
    #p2c
    set ::HANDLER($::GNERRST-$::BIT10) {processIioError 2 2}
    #p2b
    set ::HANDLER($::GNERRST-$::BIT9)  {processIioError 2 1}
    #p2a
    set ::HANDLER($::GNERRST-$::BIT8)  {processIioError 2 0}
    #p1b
    set ::HANDLER($::GNERRST-$::BIT7)  {processIioError 1 1}
    #p1a
    set ::HANDLER($::GNERRST-$::BIT6)  {processIioError 1 0}
    #DMI
    set ::HANDLER($::GNERRST-$::BIT5)  {processIioError 0 0}
    #-------------------end of iio related error handlers---------------------------------
    
    #-------------------cpu related error handlers----------------------------------------
    for { set i 0 } { $i < $::MCA_BANK_NUM } { incr i } {
        set status $[subst ::MC$i]_STATUS
        set bank [subst $status]
        set ::HANDLER($bank) "branch MC[subst $i]_STATUS VAL"
        set ::HANDLER($bank-$::VAL) "branch MC[subst $i]_STATUS UC"
        set ::HANDLER($bank-$::UC) "processCpuError $i"

        if { $i >= $::MCA_BANK_MEM_MINNUM && $i <= $::MCA_BANK_MEM_MAXNUM} then {
            set ::HANDLER($bank-$::UC) "branch MC[subst $i]_STATUS ADDRV"
            set ::HANDLER($bank-$::ADDRV) "processMemError $i 1"
            set ::HANDLER($bank-!$::ADDRV) "processMemError $i 0"
		}

        if {$::CPUTYPE eq "CPU_IVT"} {
		    if { $i == 4 || $i == 5 } then { 
			    set ::HANDLER($bank-$::UC) "processQpiError $i" 
		    }
        } elseif {$::CPUTYPE eq "CPU_HSX"} {
		    if { $i == $::MCA_BANK_QPI_PORT0_NUMBER || $i == $::MCA_BANK_QPI_PORT1_NUMBER || $i == $::MCA_BANK_QPI_PORT2_NUMBER } then { 
			    set ::HANDLER($bank-$::UC) "processQpiError $i" 
			    set ::HANDLER($bank-$::VAL) "processQpiMCAError $i" 
		    }
		}
	}
	
	#-------------------qpi related error handlers----------------------------------------
	for { set i 0 } { $i < $::MAX_QPI_PORT } { incr i } {
		set status $[subst ::QPIMC$i]_STATUS
		set bank [subst $status]
		set ::HANDLER($bank) "branch QPIMC[subst $i]_STATUS VAL"
		set ::HANDLER($bank-$::VAL) "branch QPIMC[subst $i]_STATUS UC"
		set ::HANDLER($bank-$::UC) "processQpiMCError $i"
	}
	#-------------------MB(JCK) related error handlers----------------------------------------
	set ::HANDLER($::Ha0Jck0FErr) {processMemError_MB 0}
	set ::HANDLER($::Ha1Jck0FErr) {processMemError_MB 1}
}

#--------------------------------------------------------------------------------
# parseMSR: Parse binary cpu error data
#--------------------------------------------------------------------------------
proc parseMSR { } {
    set readFileRet [read_file msr_err]
    
    if {$readFileRet} then {
        die "!->failed to read msr_err file"
    }
    
    set data [ binary format H* $::INFO(data-msr_err) ] 
    set length [string length $data]
    set log ""
    set ::MachineType [string range $data 0 6]
    set ::SerialNumber [string range $data 7 13]
    set ::TimeStamp [string range $data 14 25]
    
    set entries [expr ($length - 26) / 12]

    # RTC Issue 127400 - Workaround the spec limitation to msr_err:
    # Byte 27 :  CPU and Core number 
    #   Bit[7:4] Core Number (0-based index)
    #   Bit[3:0] (1-based index which matches system board silk screen label)
    # Only can support up to 16 Cores
    set RepeatStart 0
    set PreCpuNum 0
    set PreAddr  0
    set PreCoreNum 0

    append ::msr_log "\n===== MSR Logged from msr_err ====="
    for { set i 0 } { $i < $entries  } { incr i } {
    
        binary scan $data @[subst [expr ($i * 12) + 26]]cu1cu1su1wu1 entry cpu_core addr value
        
        set cpu_num   [expr $cpu_core & 0x0F]
        if {[info exist ::CpuMap] != 0 } {
            set cpu_num   $::CpuMap($cpu_num)
        }
        set core_num  [expr ($cpu_core & 0xF0) >> 4]
        
        # RTC Issue 127400 - Workaround the spec limitation to msr_err:
        if { $RepeatStart == 1 && $PreCpuNum == $cpu_num &&  $PreAddr == $addr } {
            set core_num [expr $core_num + 0x10]
        } else {
            set RepeatStart 0
        }
        
        if { $cpu_num > $::MAX_CPU } then {
            set ::MAX_CPU $cpu_num
        }
        
        set addr_code [format 0x%04X $addr]
        set value_code [long2hex $value]        
        set reg_store "REGISTERS_$core_num"  
        
        # treat PECI read failure as value 0 
        if {[expr $value & 0xffff00000000ffff] == 0xffff00000000ffff} { 
            set value 0
        }
        
        upvar #0 $reg_store REGS
        
        set REGS(MSR:$addr_code-$cpu_num) $value        
        set REGS(MSR:$addr_code-$cpu_num-entry) $entry
        debug "$reg_store\(MSR:$addr_code-$cpu_num\) \t= [format 0x%016lX $value]"
        #debug "MSR Entry -- CPO: $cpu_num Core: $core_num, address: $addr_code, value: $value_code"
        append ::msr_log "\nEntry:[format %03d $entry] "
        append ::msr_log "Cpu:[format %02d $cpu_num] - Core:[format %02d $core_num] " 
        append ::msr_log "MSR:$addr_code\t"
        append ::msr_log "Value:$value_code"

        # RTC Issue 127400 - Workaround the spec limitation to msr_err:
        set PreCpuNum $cpu_num
        set PreAddr $addr
        set PreCoreNum $core_num
        if { $PreCoreNum == 0x0F } {
            set RepeatStart 1
        }
    }
    append ::msr_log "\n"
    #add_log errlog $log
}

#--------------------------------------------------------------------------------
# parseCSR : Parse binary iio error data
#--------------------------------------------------------------------------------
proc parseCSR {} {
    
    set readFileRet [read_file csr_err]
    
    if {$readFileRet} then {
        die "!->failed to read csr_err file"
    }
    
    set data [ binary format H* $::INFO(data-csr_err) ]
    set length [string length $data]
    set entries [expr ($length - 26) / 12]
    
    #debug "CSR data length: $length, entries: $entries"

    append ::csr_log "\n===== CSR Logged from csr_err ====="
    for { set i 0 } { $i < $entries  } { incr i } {
        binary scan $data @[subst [expr ($i * 12) + 26]]cu1cu1Su1iu1iu1 entry iio_num cc addr value
        
        if {[info exist ::CpuMap] != 0 } {
        set iio_num   $::CpuMap($iio_num)
        }
        
        if { $iio_num > $::MAX_IIO } then {
            set ::MAX_IIO $iio_num
        }
        set addr_code [format 0x%08X $addr]
        set value_code [format 0x%08X $value]
    
        # treat PECI read failure as value 0 
        if {$cc == 0xFFFF} {
            set value 0
        }
        
        set ::REGISTERS(CSR:$addr_code-$iio_num) $value
        set ::REGISTERS(CSR:$addr_code-entry) $entry
        debug "::REGISTERS(CSR:$addr_code-$iio_num) = [format 0x%08X $value]"
        
        array set csr_addr  [decodeCSR $addr]
        #debug "CSR Entry -- IIO num: $iio_num, address: $addr_code, value: $value_code"
        append ::csr_log "\nEntry:[format %03d $entry] "
        append ::csr_log "Cpu:[format %02d $iio_num] "
        append ::csr_log "CSR:${addr_code}($csr_addr(Bus)/$csr_addr(Device)/$csr_addr(Function)/$csr_addr(Offset)) "
        append ::csr_log "Value:$value_code"
    }
    append ::csr_log "\n"
    #add_log errlog $log
}

#--------------------------------------------------------------------------------
# parseMB: Parse binary memory buffer data
#--------------------------------------------------------------------------------
proc parseMB {} {

    set readFileRet [read_file mb_err]
    if {$readFileRet} then {
        debug "!->failed to read mb_err file"
        return
    }
    
    set data $::INFO(data-mb_err)
    set data [ binary format H* $::INFO(data-mb_err) ]
    set length [string length $data]
    set entries [expr ($length - 26) / 12]
    
    append ::mb_log "\n===== JC Logged from mb_err ====="
    for { set i 0 } { $i < $entries  } { incr i } {
        binary scan $data @[subst [expr ($i * 12) + 26]]cu1cu1Su1Su1su1iu1 entry mb_num rs cc addr value
        
        if { $mb_num > $::MAX_JC } then {
            set ::MAX_JC $mb_num
        }
        set addr_code [format 0x%04X $addr]
        set value_code [format 0x%08X $value]
        if {[info exist ::CpuMap] != 0 } {
            set cpu   $::CpuMap([expr ($mb_num / 4) + 1])
        }
        set mb_num [expr $mb_num % 4]

        # treat PECI read failure as value 0 
        if {$cc == 0xFFFF} {
            set value 0
        }
        set ::REGISTERS(JCK:$mb_num-$addr_code-$cpu) $value
        debug "::REGISTERS(JCK:$mb_num-$addr_code-$cpu) = $value_code"
        
        append ::mb_log "\nEntry:[format %03d $entry] "
        append ::mb_log "Cpu:[format %02d $cpu] JC:[format %02d $mb_num] "
        append ::mb_log "Addr:$addr_code "
        append ::mb_log "Value:$value_code"
    }
    append ::mb_log "\n"
}

#--------------------------------------------------------------------------------
# processMSR : This is the entry point of processing cpu errors
#--------------------------------------------------------------------------------
proc processMSR { } {
        set ::REG_STORE "REGISTERS_0"
    
    for {set cpu 1} {$cpu <= $::MAX_CPU} {incr cpu} {
        set ::TARGET_CPU $cpu
        if {[info exist ::RevCpuMap([expr $::TARGET_CPU - 1])] == 0 } {
            continue
        }
        set ::VgpioCpuIndex [expr $::RevCpuMap([expr $::TARGET_CPU-1]) + 1]
        # MC0 - MC3
        for { set i 0 } { $i < 4 } { incr i } {
            for {set j 0} {$j < $::MAX_CORE} {incr j} {
                set ::REG_STORE "REGISTERS_$j"
                if { [catch {check [subst MC$i]_STATUS} fid]} {
                    debug "MC$i: $fid"
                }
            }   
            set ::REG_STORE "REGISTERS_0"
        }

        # MC4 - MC31
        for { set i 4 } { $i < $::MCA_BANK_NUM } { incr i } {
            if { [catch {check [subst MC$i]_STATUS} fid]} {
                debug "MC${i}: $fid"
            }
        }

        # QPI_MC0 - QPI_MC2
        for { set i 0 } { $i < $::MAX_QPI_PORT } { incr i } {
            if { [catch {check [subst QPIMC$i]_STATUS} fid]} {
                debug "MC$i: $fid"
            }
        }
    }
}

#--------------------------------------------------------------------------------
# processCSR : This is the entry point of processing iio errors
#--------------------------------------------------------------------------------
proc processCSR { } {
    set ::REG_STORE "REGISTERS"
    for {set cpu 1} {$cpu <= $::MAX_IIO} {incr cpu} {
        #debug "check iio of cpu numer $cpu" 
        set ::TARGET_CPU $cpu
        check GSYSST 
        #check GFERRST
        #check GNERRST
    }
}

proc processMB { } {
	set ::REG_STORE "REGISTERS"
	for {set cpu 1} {$cpu <= $::MAX_CPU} {incr cpu} {
		debug "check MB of cpu numer $cpu" 
		set ::TARGET_CPU $cpu
		check Ha0Jck0FErr
		check Ha1Jck0FErr
	}
}

proc createLog {} {
    # Printing out header into our log file
    add_log errlog "MachineTypeModel = $::MachineType"
    add_log errlog "SerialNumber = $::SerialNumber"
    add_log errlog "TimeStamp (MMDDYYHHMMSS) = $::TimeStamp"
    
    #log all event we found here
    #add_log errlog "Total event found:"
    #foreach {item} $::VGPIO  {
    #   foreach {event vgpio auxdata} $item {
    #   set temp [subst $vgpio]
    #   add_log errlog "vgpio $temp"
    #   }
    #}

    #log the actual event we asserted.
    add_log errlog "VGPIO asserted"
    foreach {vgpio} $::VGPIO_LOG_BUFFER {
        set temp [subst $vgpio]
        add_log errlog "vgpio $temp"
        add_log vgpio_log "vgpio $temp"
    }
    
    add_log errlog $::msr_log
    add_log errlog $::csr_log
    if {[info exist ::mb_log]} {
      add_log errlog $::mb_log
    }

    add_log errlog "===== QPI Poisoning Logged ====="
    if { [info exist ::QPIpoison_log] } {
        add_log errlog $::QPIpoison_log
    }
}

proc setFRUMap {} {

	#PCIMap array indexed as ::PCIMap(socket-port-bifurcated_num)
	#No need to provide PCIMAP as CreateTclPciMap() will provide the info from gIbmSysCfgProtocolGuid

	#DIMMMap array indexed as DIMMMap(Cpu-Channel-Dimm).
	#Note that dimm numbers are repeated in each socket (Cpu/memory board)
	#Rule of channel number used here is Channel = iMC * 4 + SMI2Port * 2 + JC_SubChannel
	if {$::PLATFORM eq "ANDROMEDA"} {
		for {set cpu 0} { $cpu < 4 } {incr cpu} {
			set ::DIMMMap($cpu-3-0) 1	
			set ::DIMMMap($cpu-3-1) 2		
			set ::DIMMMap($cpu-3-2) 3	
			set ::DIMMMap($cpu-4-2) 4	
			set ::DIMMMap($cpu-4-1) 5	
			set ::DIMMMap($cpu-4-0) 6	
			set ::DIMMMap($cpu-0-2) 7	
			set ::DIMMMap($cpu-0-1) 8	
			set ::DIMMMap($cpu-0-0) 9	
			set ::DIMMMap($cpu-7-0) 10	
			set ::DIMMMap($cpu-7-1) 11	
			set ::DIMMMap($cpu-7-2) 12	
			set ::DIMMMap($cpu-1-2) 13	
			set ::DIMMMap($cpu-1-1) 14	
			set ::DIMMMap($cpu-1-0) 15	
			set ::DIMMMap($cpu-6-0) 16	
			set ::DIMMMap($cpu-6-1) 17	
			set ::DIMMMap($cpu-6-2) 18	
			set ::DIMMMap($cpu-2-0) 19	
			set ::DIMMMap($cpu-2-1) 20	
			set ::DIMMMap($cpu-2-2) 21	
			set ::DIMMMap($cpu-5-2) 22	
			set ::DIMMMap($cpu-5-1) 23	
			set ::DIMMMap($cpu-5-0) 24	
		}
	} elseif {$::PLATFORM eq "NANTAHALA"} {
		set ::DIMMMap(0-3-0) 1
		set ::DIMMMap(0-3-1) 2
		set ::DIMMMap(0-3-2) 3
		set ::DIMMMap(0-2-0) 4
		set ::DIMMMap(0-2-1) 5
		set ::DIMMMap(0-2-2) 6
		set ::DIMMMap(0-7-0) 7
		set ::DIMMMap(0-7-1) 8
		set ::DIMMMap(0-7-2) 9
		set ::DIMMMap(0-6-0) 10
		set ::DIMMMap(0-6-1) 11
		set ::DIMMMap(0-6-2) 12
		set ::DIMMMap(1-0-2) 13
		set ::DIMMMap(1-0-1) 14
		set ::DIMMMap(1-0-0) 15
		set ::DIMMMap(1-1-2) 16
		set ::DIMMMap(1-1-1) 17
		set ::DIMMMap(1-1-0) 18
		set ::DIMMMap(1-4-2) 19
		set ::DIMMMap(1-4-1) 20
		set ::DIMMMap(1-4-0) 21
		set ::DIMMMap(1-5-2) 22
		set ::DIMMMap(1-5-1) 23
		set ::DIMMMap(1-5-0) 24
		set ::DIMMMap(0-1-0) 25
		set ::DIMMMap(0-1-1) 26
		set ::DIMMMap(0-1-2) 27
		set ::DIMMMap(0-0-0) 28
		set ::DIMMMap(0-0-1) 29
		set ::DIMMMap(0-0-2) 30
		set ::DIMMMap(0-4-2) 31
		set ::DIMMMap(0-4-1) 32
		set ::DIMMMap(0-4-0) 33
		set ::DIMMMap(0-5-2) 34
		set ::DIMMMap(0-5-1) 35
		set ::DIMMMap(0-5-0) 36
		set ::DIMMMap(1-3-0) 37
		set ::DIMMMap(1-3-1) 38
		set ::DIMMMap(1-3-2) 39
		set ::DIMMMap(1-2-0) 40
		set ::DIMMMap(1-2-1) 41
		set ::DIMMMap(1-2-2) 42
		set ::DIMMMap(1-6-2) 43
		set ::DIMMMap(1-6-1) 44
		set ::DIMMMap(1-6-0) 45
		set ::DIMMMap(1-7-2) 46
		set ::DIMMMap(1-7-1) 47
		set ::DIMMMap(1-7-0) 48
	}
}

proc setCpuMap {} {
    array unset ::CpuMap
	set readFileRet [read_file csr_err]
	
	if {$readFileRet} then {
		die "!->failed to read csr_err file"
	}
	
	set data [ binary format H* $::INFO(data-csr_err) ]
	set length [string length $data]
	set entries [expr ($length - 26) / 12]
	
	debug "CSR data length: $length, entries: $entries"

	for { set i 0 } { $i < $entries  } { incr i } {
		binary scan $data @[subst [expr ($i * 12) + 26]]cu1cu1Su1iu1iu1 entry iio_num cc addr value
		
		set addr_code [format 0x%08X $addr]
		set value_code [format 0x%08X $value]
	
		# treat PECI read failure as value 0 
		if {$cc == 0xFFFF} {
		    set value 0
        }
		
		if {"CSR:$addr_code" eq $::CPUNodeId} then {
			debug "$::CPUNodeId = $value"
			set local_node_id [expr ($value & 0x7) + 1]
			set ::CpuMap($iio_num) $local_node_id
			debug "CpuMap($iio_num) = $local_node_id"
		}
	}
}

proc setReverseCpuMap {} {
    
    if {[info exist ::CpuMap] == 0 } {
        for {set i 0} {$i < 4} {incr i} {
            set ::RevCpuMap(%i) $i
        }
    } else {
        for {set i 1} {$i <= 8} {incr i} {
            if {[info exist ::CpuMap($i)]} {
                set ::RevCpuMap([expr $::CpuMap($i) - 1]) [expr $i - 1]
                debug "RevCpuMap([expr $::CpuMap($i) - 1]) =  [expr $i - 1]"
            } else {
                return
            }
        }
    }
}

proc collectRemoteError {} {
    debug "Try collecting temp error from node 1 to [expr $::NodeCount-1]"
    
    for {set i 1} {$i < $::NodeCount} {incr i} {
        set fileid [subst vgpio_tmp$i]
        debug "reading $fileid"
        set check [read_file $fileid]
        if {$check} then {
            debug "failed to read $fileid file"
            continue
        } 
        
        set data $::INFO(data-$fileid)
        set vgpios [binary format H* $::INFO(data-$fileid) ]
        #debug "vgpios=$vgpios"
        set VgpioList [split $vgpios "\n"]
        #debug "VgpioList=$VgpioList"
        set VgpioNumber [llength $VgpioList]
        #debug "VgpioNumber=$VgpioNumber"
        for {set VgpioIndex 0} {$VgpioIndex < $VgpioNumber} {incr VgpioIndex} {
          set VgpioCommands [split [lindex $VgpioList $VgpioIndex]]
          set head [lindex $VgpioCommands 0]
          set errType [lindex $VgpioCommands 1]
          set vgpio [lindex $VgpioCommands 2]
          set eid [lindex $VgpioCommands 3]
          set extended_data [join [lrange $VgpioCommands 4 end]]
          debug ">>> head:$head errType:$errType vgpio:$vgpio eid:$eid extended_data:$extended_data "
          lappend ::VGPIO [list $i $errType "$vgpio $eid" $extended_data]
          #debug "VGPIO=$::VGPIO"
        }
    }
}

proc readConfirmedError {} {
    set fileid [subst vgpio_out]
    set check [read_file $fileid]

    if {$check} then {
        debug "failed to read $fileid file"
        return
    } 
    
    set data $::INFO(data-$fileid)  
    set vgpios [binary format H* $::INFO(data-$fileid) ]    
        
    foreach {errType vgpio eid } $vgpios {
        lappend ::FINAL_VGPIO "$vgpio $eid"
    }
}

proc getExecutionEnv {} {
    # TCL parameter example from IMM: 
    # With 4 nodes system
    # -C 4 -N 0 : Primary Node 
    # -C 4 -N 1 : 2nd Node
    # -C 4 -N 2 : 3rd Node
    # -C 4 -N 3 : 4th Node

    # 0-based Node ID from IMM by -N parameter
    set ::NodeID        [get_multi_node_id]
    # -C paramenter
    set ::NodeCount     [get_multi_node_count]
    set ::RemotePhase       [is_remote_node]
    set ::IsPrimary     [expr ($::NodeID == 0)]
    set ::IsMultiNode   [expr ($::NodeCount != 0)]
}

#--------------------------------------------------------------------------------
# main()
#-------------------------------------------------------------------------------
proc main {} {
    
    if {[catch getExecutionEnv]} {
        set ::NodeID  0 
        set ::NodeCount 0
        set ::RemotePhase 0
        set ::IsPrimary     [expr ($::NodeID == 0)]
        set ::IsMultiNode   [expr ($::NodeCount != 0)]
    }
    
    debug "IsPrimary: $::IsPrimary. IsMultiNode: $::IsMultiNode NodeCount: $::NodeCount NodeID: $::NodeID RemotePhase: $::RemotePhase"

    initializePciAddress
    
    initialize
    setFRUMap
    setupHandler
    
	setCpuMap
	
	setReverseCpuMap
    parseMSR
    parseCSR
    parseMB

    processMSR
    processCSR
    processMB
    
    if {$::IsMultiNode} {
        if {$::IsPrimary} {
            collectRemoteError
        } elseif {$::RemotePhase} {
            readConfirmedError
        }
    }
    
    if {[catch fireVGPIO fid]} {
        debug "error occured in fireVGPIO: $fid"
    }
    
    createLog
    return
}

array unset ::REGISTERS
set ::REGISTERS(CSR:0x0017C050-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x050)
set ::REGISTERS(CSR:0x0017C054-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x054)
set ::REGISTERS(CSR:0x0017C058-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x058)
set ::REGISTERS(CSR:0x0017C05C-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x05C)
set ::REGISTERS(CSR:0x000280A8-1) 0xBC000000 ;# PCI (0x00, 0x05, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x000280AC-1) 0xBFF00000 ;# PCI (0x00, 0x05, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x0017D0C0-1) 0xC0000001 ;# PCI (0x01, 0x0F, 0x05, 0x0C0)
set ::REGISTERS(CSR:0x0017D0C4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0C4)
set ::REGISTERS(CSR:0x0017D040-1) 0xDB000069 ;# PCI (0x01, 0x0F, 0x05, 0x040)
set ::REGISTERS(CSR:0x0017D044-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x044)
set ::REGISTERS(CSR:0x0017D048-1) 0xE700006F ;# PCI (0x01, 0x0F, 0x05, 0x048)
set ::REGISTERS(CSR:0x0017D04C-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x04C)
set ::REGISTERS(CSR:0x0017D050-1) 0xF3000075 ;# PCI (0x01, 0x0F, 0x05, 0x050)
set ::REGISTERS(CSR:0x0017D054-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x054)
set ::REGISTERS(CSR:0x0017D058-1) 0xFB00007B ;# PCI (0x01, 0x0F, 0x05, 0x058)
set ::REGISTERS(CSR:0x0017D05C-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x05C)
set ::REGISTERS(CSR:0x0017D060-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x060)
set ::REGISTERS(CSR:0x0017D064-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x064)
set ::REGISTERS(CSR:0x0017D068-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x068)
set ::REGISTERS(CSR:0x0017D06C-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x06C)
set ::REGISTERS(CSR:0x0017D070-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x070)
set ::REGISTERS(CSR:0x0017D074-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x074)
set ::REGISTERS(CSR:0x0017D078-1) 0xFF00007F ;# PCI (0x01, 0x0F, 0x05, 0x078)
set ::REGISTERS(CSR:0x0017D07C-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x07C)
set ::REGISTERS(CSR:0x0017D080-1) 0xFF010001 ;# PCI (0x01, 0x0F, 0x05, 0x080)
set ::REGISTERS(CSR:0x0017D084-1) 0x0000023F ;# PCI (0x01, 0x0F, 0x05, 0x084)
set ::REGISTERS(CSR:0x0017D088-1) 0xFF012001 ;# PCI (0x01, 0x0F, 0x05, 0x088)
set ::REGISTERS(CSR:0x0017D08C-1) 0x0000027F ;# PCI (0x01, 0x0F, 0x05, 0x08C)
set ::REGISTERS(CSR:0x0017D090-1) 0xFF014001 ;# PCI (0x01, 0x0F, 0x05, 0x090)
set ::REGISTERS(CSR:0x0017D094-1) 0x000002BF ;# PCI (0x01, 0x0F, 0x05, 0x094)
set ::REGISTERS(CSR:0x0017D098-1) 0xFF016001 ;# PCI (0x01, 0x0F, 0x05, 0x098)
set ::REGISTERS(CSR:0x0017D09C-1) 0x000002FF ;# PCI (0x01, 0x0F, 0x05, 0x09C)
set ::REGISTERS(CSR:0x0017D0A0-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A0)
set ::REGISTERS(CSR:0x0017D0A4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A4)
set ::REGISTERS(CSR:0x0017D0A8-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A8)
set ::REGISTERS(CSR:0x0017D0AC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0AC)
set ::REGISTERS(CSR:0x0017D0B0-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B0)
set ::REGISTERS(CSR:0x0017D0B4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B4)
set ::REGISTERS(CSR:0x0017D0B8-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B8)
set ::REGISTERS(CSR:0x0017D0BC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0BC)
set ::REGISTERS(CSR:0x0017C060-1) 0x040203C3 ;# PCI (0x01, 0x0F, 0x04, 0x060)
set ::REGISTERS(CSR:0x0017C068-1) 0x040403C3 ;# PCI (0x01, 0x0F, 0x04, 0x068)
set ::REGISTERS(CSR:0x0017C070-1) 0x040603C3 ;# PCI (0x01, 0x0F, 0x04, 0x070)
set ::REGISTERS(CSR:0x0017C078-1) 0x040803C3 ;# PCI (0x01, 0x0F, 0x04, 0x078)
set ::REGISTERS(CSR:0x0017C080-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x080)
set ::REGISTERS(CSR:0x0017C088-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x088)
set ::REGISTERS(CSR:0x0017C090-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x090)
set ::REGISTERS(CSR:0x0017C098-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x098)
set ::REGISTERS(CSR:0x0017C0A0-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A0)
set ::REGISTERS(CSR:0x0017C0A8-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A8)
set ::REGISTERS(CSR:0x0017C0B0-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B0)
set ::REGISTERS(CSR:0x0017C0B8-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B8)
set ::REGISTERS(CSR:0x0017C0C0-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C0)
set ::REGISTERS(CSR:0x0017C0C8-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C8)
set ::REGISTERS(CSR:0x0017C0D0-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D0)
set ::REGISTERS(CSR:0x0017C0D8-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D8)
set ::REGISTERS(CSR:0x0017C0E0-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E0)
set ::REGISTERS(CSR:0x0017C0E8-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E8)
set ::REGISTERS(CSR:0x0017C0F0-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F0)
set ::REGISTERS(CSR:0x0017C0F8-1) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F8)
set ::REGISTERS(CSR:0x0017C064-1) 0x44004400 ;# PCI (0x01, 0x0F, 0x04, 0x064)
set ::REGISTERS(CSR:0x0017C06C-1) 0x55115511 ;# PCI (0x01, 0x0F, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0017C074-1) 0x66226622 ;# PCI (0x01, 0x0F, 0x04, 0x074)
set ::REGISTERS(CSR:0x0017C07C-1) 0x77337733 ;# PCI (0x01, 0x0F, 0x04, 0x07C)
set ::REGISTERS(CSR:0x0017C084-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x084)
set ::REGISTERS(CSR:0x0017C08C-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x08C)
set ::REGISTERS(CSR:0x0017C094-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x094)
set ::REGISTERS(CSR:0x0017C09C-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x09C)
set ::REGISTERS(CSR:0x0017C0A4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0A4)
set ::REGISTERS(CSR:0x0017C0AC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0AC)
set ::REGISTERS(CSR:0x0017C0B4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0B4)
set ::REGISTERS(CSR:0x0017C0BC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0BC)
set ::REGISTERS(CSR:0x0017C0C4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0C4)
set ::REGISTERS(CSR:0x0017C0CC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0CC)
set ::REGISTERS(CSR:0x0017C0D4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0D4)
set ::REGISTERS(CSR:0x0017C0DC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0DC)
set ::REGISTERS(CSR:0x0017C0E4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0E4)
set ::REGISTERS(CSR:0x0017C0EC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0EC)
set ::REGISTERS(CSR:0x0017C0F4-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0F4)
set ::REGISTERS(CSR:0x0017C0FC-1) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0FC)
set ::REGISTERS(CSR:0x00190080-1) 0x0F308003 ;# PCI (0x01, 0x12, 0x00, 0x080)
set ::REGISTERS(CSR:0x00190084-1) 0x45400B88 ;# PCI (0x01, 0x12, 0x00, 0x084)
set ::REGISTERS(CSR:0x00190040-1) 0x0002F7E4 ;# PCI (0x01, 0x12, 0x00, 0x040)
set ::REGISTERS(CSR:0x00190044-1) 0x0080F7E4 ;# PCI (0x01, 0x12, 0x00, 0x044)
set ::REGISTERS(CSR:0x00190048-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x048)
set ::REGISTERS(CSR:0x0019004C-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x04C)
set ::REGISTERS(CSR:0x00190050-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x050)
set ::REGISTERS(CSR:0x00190054-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x054)
set ::REGISTERS(CSR:0x00190058-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x058)
set ::REGISTERS(CSR:0x0019005C-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x05C)
set ::REGISTERS(CSR:0x00190060-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x060)
set ::REGISTERS(CSR:0x00190064-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x064)
set ::REGISTERS(CSR:0x00190068-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x068)
set ::REGISTERS(CSR:0x0019006C-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x00, 0x06C)
set ::REGISTERS(CSR:0x00194080-1) 0x0F308003 ;# PCI (0x01, 0x12, 0x04, 0x080)
set ::REGISTERS(CSR:0x00194084-1) 0x45400B88 ;# PCI (0x01, 0x12, 0x04, 0x084)
set ::REGISTERS(CSR:0x00194040-1) 0x0002F7E4 ;# PCI (0x01, 0x12, 0x04, 0x040)
set ::REGISTERS(CSR:0x00194044-1) 0x0080F7E4 ;# PCI (0x01, 0x12, 0x04, 0x044)
set ::REGISTERS(CSR:0x00194048-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x048)
set ::REGISTERS(CSR:0x0019404C-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x04C)
set ::REGISTERS(CSR:0x00194050-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x050)
set ::REGISTERS(CSR:0x00194054-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x054)
set ::REGISTERS(CSR:0x00194058-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x058)
set ::REGISTERS(CSR:0x0019405C-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x05C)
set ::REGISTERS(CSR:0x00194060-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x060)
set ::REGISTERS(CSR:0x00194064-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x064)
set ::REGISTERS(CSR:0x00194068-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x068)
set ::REGISTERS(CSR:0x0019406C-1) 0x0080F000 ;# PCI (0x01, 0x12, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0019807C-1) 0x00013D0D ;# PCI (0x01, 0x13, 0x00, 0x07C)
set ::REGISTERS(CSR:0x00198080-1) 0x0002F7E4 ;# PCI (0x01, 0x13, 0x00, 0x080)
set ::REGISTERS(CSR:0x00198084-1) 0x0080F7E4 ;# PCI (0x01, 0x13, 0x00, 0x084)
set ::REGISTERS(CSR:0x00198088-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x088)
set ::REGISTERS(CSR:0x0019808C-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x08C)
set ::REGISTERS(CSR:0x00198090-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x090)
set ::REGISTERS(CSR:0x00198094-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x094)
set ::REGISTERS(CSR:0x00198098-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x098)
set ::REGISTERS(CSR:0x0019809C-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001980A0-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001980A4-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001980A8-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001980AC-1) 0x0080F000 ;# PCI (0x01, 0x13, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x001B007C-1) 0x00013D0D ;# PCI (0x01, 0x16, 0x00, 0x07C)
set ::REGISTERS(CSR:0x001B0080-1) 0x0002F7E4 ;# PCI (0x01, 0x16, 0x00, 0x080)
set ::REGISTERS(CSR:0x001B0084-1) 0x0080F7E4 ;# PCI (0x01, 0x16, 0x00, 0x084)
set ::REGISTERS(CSR:0x001B0088-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x088)
set ::REGISTERS(CSR:0x001B008C-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x08C)
set ::REGISTERS(CSR:0x001B0090-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x090)
set ::REGISTERS(CSR:0x001B0094-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x094)
set ::REGISTERS(CSR:0x001B0098-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x098)
set ::REGISTERS(CSR:0x001B009C-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001B00A0-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001B00A4-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001B00A8-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001B00AC-1) 0x0080F000 ;# PCI (0x01, 0x16, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x0017C050-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x050)
set ::REGISTERS(CSR:0x0017C054-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x054)
set ::REGISTERS(CSR:0x0017C058-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x058)
set ::REGISTERS(CSR:0x0017C05C-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x05C)
set ::REGISTERS(CSR:0x000280A8-2) 0xBC000000 ;# PCI (0x00, 0x05, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x000280AC-2) 0xBFF00000 ;# PCI (0x00, 0x05, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x0017D0C0-2) 0xC0000001 ;# PCI (0x01, 0x0F, 0x05, 0x0C0)
set ::REGISTERS(CSR:0x0017D0C4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0C4)
set ::REGISTERS(CSR:0x0017D040-2) 0xDB000069 ;# PCI (0x01, 0x0F, 0x05, 0x040)
set ::REGISTERS(CSR:0x0017D044-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x044)
set ::REGISTERS(CSR:0x0017D048-2) 0xE700006F ;# PCI (0x01, 0x0F, 0x05, 0x048)
set ::REGISTERS(CSR:0x0017D04C-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x04C)
set ::REGISTERS(CSR:0x0017D050-2) 0xF3000075 ;# PCI (0x01, 0x0F, 0x05, 0x050)
set ::REGISTERS(CSR:0x0017D054-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x054)
set ::REGISTERS(CSR:0x0017D058-2) 0xFB00007B ;# PCI (0x01, 0x0F, 0x05, 0x058)
set ::REGISTERS(CSR:0x0017D05C-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x05C)
set ::REGISTERS(CSR:0x0017D060-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x060)
set ::REGISTERS(CSR:0x0017D064-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x064)
set ::REGISTERS(CSR:0x0017D068-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x068)
set ::REGISTERS(CSR:0x0017D06C-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x06C)
set ::REGISTERS(CSR:0x0017D070-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x070)
set ::REGISTERS(CSR:0x0017D074-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x074)
set ::REGISTERS(CSR:0x0017D078-2) 0xFF00007F ;# PCI (0x01, 0x0F, 0x05, 0x078)
set ::REGISTERS(CSR:0x0017D07C-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x07C)
set ::REGISTERS(CSR:0x0017D080-2) 0xFF010001 ;# PCI (0x01, 0x0F, 0x05, 0x080)
set ::REGISTERS(CSR:0x0017D084-2) 0x0000023F ;# PCI (0x01, 0x0F, 0x05, 0x084)
set ::REGISTERS(CSR:0x0017D088-2) 0xFF012001 ;# PCI (0x01, 0x0F, 0x05, 0x088)
set ::REGISTERS(CSR:0x0017D08C-2) 0x0000027F ;# PCI (0x01, 0x0F, 0x05, 0x08C)
set ::REGISTERS(CSR:0x0017D090-2) 0xFF014001 ;# PCI (0x01, 0x0F, 0x05, 0x090)
set ::REGISTERS(CSR:0x0017D094-2) 0x000002BF ;# PCI (0x01, 0x0F, 0x05, 0x094)
set ::REGISTERS(CSR:0x0017D098-2) 0xFF016001 ;# PCI (0x01, 0x0F, 0x05, 0x098)
set ::REGISTERS(CSR:0x0017D09C-2) 0x000002FF ;# PCI (0x01, 0x0F, 0x05, 0x09C)
set ::REGISTERS(CSR:0x0017D0A0-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A0)
set ::REGISTERS(CSR:0x0017D0A4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A4)
set ::REGISTERS(CSR:0x0017D0A8-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A8)
set ::REGISTERS(CSR:0x0017D0AC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0AC)
set ::REGISTERS(CSR:0x0017D0B0-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B0)
set ::REGISTERS(CSR:0x0017D0B4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B4)
set ::REGISTERS(CSR:0x0017D0B8-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B8)
set ::REGISTERS(CSR:0x0017D0BC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0BC)
set ::REGISTERS(CSR:0x0017C060-2) 0x040203C3 ;# PCI (0x01, 0x0F, 0x04, 0x060)
set ::REGISTERS(CSR:0x0017C068-2) 0x040403C3 ;# PCI (0x01, 0x0F, 0x04, 0x068)
set ::REGISTERS(CSR:0x0017C070-2) 0x040603C3 ;# PCI (0x01, 0x0F, 0x04, 0x070)
set ::REGISTERS(CSR:0x0017C078-2) 0x040803C3 ;# PCI (0x01, 0x0F, 0x04, 0x078)
set ::REGISTERS(CSR:0x0017C080-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x080)
set ::REGISTERS(CSR:0x0017C088-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x088)
set ::REGISTERS(CSR:0x0017C090-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x090)
set ::REGISTERS(CSR:0x0017C098-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x098)
set ::REGISTERS(CSR:0x0017C0A0-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A0)
set ::REGISTERS(CSR:0x0017C0A8-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A8)
set ::REGISTERS(CSR:0x0017C0B0-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B0)
set ::REGISTERS(CSR:0x0017C0B8-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B8)
set ::REGISTERS(CSR:0x0017C0C0-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C0)
set ::REGISTERS(CSR:0x0017C0C8-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C8)
set ::REGISTERS(CSR:0x0017C0D0-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D0)
set ::REGISTERS(CSR:0x0017C0D8-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D8)
set ::REGISTERS(CSR:0x0017C0E0-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E0)
set ::REGISTERS(CSR:0x0017C0E8-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E8)
set ::REGISTERS(CSR:0x0017C0F0-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F0)
set ::REGISTERS(CSR:0x0017C0F8-2) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F8)
set ::REGISTERS(CSR:0x0017C064-2) 0x44004400 ;# PCI (0x01, 0x0F, 0x04, 0x064)
set ::REGISTERS(CSR:0x0017C06C-2) 0x55115511 ;# PCI (0x01, 0x0F, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0017C074-2) 0x66226622 ;# PCI (0x01, 0x0F, 0x04, 0x074)
set ::REGISTERS(CSR:0x0017C07C-2) 0x77337733 ;# PCI (0x01, 0x0F, 0x04, 0x07C)
set ::REGISTERS(CSR:0x0017C084-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x084)
set ::REGISTERS(CSR:0x0017C08C-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x08C)
set ::REGISTERS(CSR:0x0017C094-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x094)
set ::REGISTERS(CSR:0x0017C09C-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x09C)
set ::REGISTERS(CSR:0x0017C0A4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0A4)
set ::REGISTERS(CSR:0x0017C0AC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0AC)
set ::REGISTERS(CSR:0x0017C0B4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0B4)
set ::REGISTERS(CSR:0x0017C0BC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0BC)
set ::REGISTERS(CSR:0x0017C0C4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0C4)
set ::REGISTERS(CSR:0x0017C0CC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0CC)
set ::REGISTERS(CSR:0x0017C0D4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0D4)
set ::REGISTERS(CSR:0x0017C0DC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0DC)
set ::REGISTERS(CSR:0x0017C0E4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0E4)
set ::REGISTERS(CSR:0x0017C0EC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0EC)
set ::REGISTERS(CSR:0x0017C0F4-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0F4)
set ::REGISTERS(CSR:0x0017C0FC-2) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0FC)
set ::REGISTERS(CSR:0x00190080-2) 0x0F308003 ;# PCI (0x01, 0x12, 0x00, 0x080)
set ::REGISTERS(CSR:0x00190084-2) 0x45400B88 ;# PCI (0x01, 0x12, 0x00, 0x084)
set ::REGISTERS(CSR:0x00190040-2) 0x0100F7E4 ;# PCI (0x01, 0x12, 0x00, 0x040)
set ::REGISTERS(CSR:0x00190044-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x044)
set ::REGISTERS(CSR:0x00190048-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x048)
set ::REGISTERS(CSR:0x0019004C-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x04C)
set ::REGISTERS(CSR:0x00190050-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x050)
set ::REGISTERS(CSR:0x00190054-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x054)
set ::REGISTERS(CSR:0x00190058-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x058)
set ::REGISTERS(CSR:0x0019005C-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x05C)
set ::REGISTERS(CSR:0x00190060-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x060)
set ::REGISTERS(CSR:0x00190064-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x064)
set ::REGISTERS(CSR:0x00190068-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x068)
set ::REGISTERS(CSR:0x0019006C-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x00, 0x06C)
set ::REGISTERS(CSR:0x00194080-2) 0x0F308003 ;# PCI (0x01, 0x12, 0x04, 0x080)
set ::REGISTERS(CSR:0x00194084-2) 0x45400B88 ;# PCI (0x01, 0x12, 0x04, 0x084)
set ::REGISTERS(CSR:0x00194040-2) 0x0100F7E4 ;# PCI (0x01, 0x12, 0x04, 0x040)
set ::REGISTERS(CSR:0x00194044-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x044)
set ::REGISTERS(CSR:0x00194048-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x048)
set ::REGISTERS(CSR:0x0019404C-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x04C)
set ::REGISTERS(CSR:0x00194050-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x050)
set ::REGISTERS(CSR:0x00194054-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x054)
set ::REGISTERS(CSR:0x00194058-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x058)
set ::REGISTERS(CSR:0x0019405C-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x05C)
set ::REGISTERS(CSR:0x00194060-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x060)
set ::REGISTERS(CSR:0x00194064-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x064)
set ::REGISTERS(CSR:0x00194068-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x068)
set ::REGISTERS(CSR:0x0019406C-2) 0x0100F000 ;# PCI (0x01, 0x12, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0019807C-2) 0x00013D0D ;# PCI (0x01, 0x13, 0x00, 0x07C)
set ::REGISTERS(CSR:0x00198080-2) 0x0100F7E4 ;# PCI (0x01, 0x13, 0x00, 0x080)
set ::REGISTERS(CSR:0x00198084-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x084)
set ::REGISTERS(CSR:0x00198088-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x088)
set ::REGISTERS(CSR:0x0019808C-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x08C)
set ::REGISTERS(CSR:0x00198090-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x090)
set ::REGISTERS(CSR:0x00198094-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x094)
set ::REGISTERS(CSR:0x00198098-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x098)
set ::REGISTERS(CSR:0x0019809C-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001980A0-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001980A4-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001980A8-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001980AC-2) 0x0100F000 ;# PCI (0x01, 0x13, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x001B007C-2) 0x00013D0D ;# PCI (0x01, 0x16, 0x00, 0x07C)
set ::REGISTERS(CSR:0x001B0080-2) 0x0100F7E4 ;# PCI (0x01, 0x16, 0x00, 0x080)
set ::REGISTERS(CSR:0x001B0084-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x084)
set ::REGISTERS(CSR:0x001B0088-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x088)
set ::REGISTERS(CSR:0x001B008C-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x08C)
set ::REGISTERS(CSR:0x001B0090-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x090)
set ::REGISTERS(CSR:0x001B0094-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x094)
set ::REGISTERS(CSR:0x001B0098-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x098)
set ::REGISTERS(CSR:0x001B009C-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001B00A0-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001B00A4-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001B00A8-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001B00AC-2) 0x0100F000 ;# PCI (0x01, 0x16, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x0017C050-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x050)
set ::REGISTERS(CSR:0x0017C054-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x054)
set ::REGISTERS(CSR:0x0017C058-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x058)
set ::REGISTERS(CSR:0x0017C05C-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x05C)
set ::REGISTERS(CSR:0x000280A8-3) 0xBC000000 ;# PCI (0x00, 0x05, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x000280AC-3) 0xBFF00000 ;# PCI (0x00, 0x05, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x0017D0C0-3) 0xC0000001 ;# PCI (0x01, 0x0F, 0x05, 0x0C0)
set ::REGISTERS(CSR:0x0017D0C4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0C4)
set ::REGISTERS(CSR:0x0017D040-3) 0xDB000069 ;# PCI (0x01, 0x0F, 0x05, 0x040)
set ::REGISTERS(CSR:0x0017D044-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x044)
set ::REGISTERS(CSR:0x0017D048-3) 0xE700006F ;# PCI (0x01, 0x0F, 0x05, 0x048)
set ::REGISTERS(CSR:0x0017D04C-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x04C)
set ::REGISTERS(CSR:0x0017D050-3) 0xF3000075 ;# PCI (0x01, 0x0F, 0x05, 0x050)
set ::REGISTERS(CSR:0x0017D054-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x054)
set ::REGISTERS(CSR:0x0017D058-3) 0xFB00007B ;# PCI (0x01, 0x0F, 0x05, 0x058)
set ::REGISTERS(CSR:0x0017D05C-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x05C)
set ::REGISTERS(CSR:0x0017D060-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x060)
set ::REGISTERS(CSR:0x0017D064-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x064)
set ::REGISTERS(CSR:0x0017D068-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x068)
set ::REGISTERS(CSR:0x0017D06C-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x06C)
set ::REGISTERS(CSR:0x0017D070-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x070)
set ::REGISTERS(CSR:0x0017D074-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x074)
set ::REGISTERS(CSR:0x0017D078-3) 0xFF00007F ;# PCI (0x01, 0x0F, 0x05, 0x078)
set ::REGISTERS(CSR:0x0017D07C-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x07C)
set ::REGISTERS(CSR:0x0017D080-3) 0xFF010001 ;# PCI (0x01, 0x0F, 0x05, 0x080)
set ::REGISTERS(CSR:0x0017D084-3) 0x0000023F ;# PCI (0x01, 0x0F, 0x05, 0x084)
set ::REGISTERS(CSR:0x0017D088-3) 0xFF012001 ;# PCI (0x01, 0x0F, 0x05, 0x088)
set ::REGISTERS(CSR:0x0017D08C-3) 0x0000027F ;# PCI (0x01, 0x0F, 0x05, 0x08C)
set ::REGISTERS(CSR:0x0017D090-3) 0xFF014001 ;# PCI (0x01, 0x0F, 0x05, 0x090)
set ::REGISTERS(CSR:0x0017D094-3) 0x000002BF ;# PCI (0x01, 0x0F, 0x05, 0x094)
set ::REGISTERS(CSR:0x0017D098-3) 0xFF016001 ;# PCI (0x01, 0x0F, 0x05, 0x098)
set ::REGISTERS(CSR:0x0017D09C-3) 0x000002FF ;# PCI (0x01, 0x0F, 0x05, 0x09C)
set ::REGISTERS(CSR:0x0017D0A0-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A0)
set ::REGISTERS(CSR:0x0017D0A4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A4)
set ::REGISTERS(CSR:0x0017D0A8-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A8)
set ::REGISTERS(CSR:0x0017D0AC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0AC)
set ::REGISTERS(CSR:0x0017D0B0-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B0)
set ::REGISTERS(CSR:0x0017D0B4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B4)
set ::REGISTERS(CSR:0x0017D0B8-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B8)
set ::REGISTERS(CSR:0x0017D0BC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0BC)
set ::REGISTERS(CSR:0x0017C060-3) 0x040203C3 ;# PCI (0x01, 0x0F, 0x04, 0x060)
set ::REGISTERS(CSR:0x0017C068-3) 0x040403C3 ;# PCI (0x01, 0x0F, 0x04, 0x068)
set ::REGISTERS(CSR:0x0017C070-3) 0x040603C3 ;# PCI (0x01, 0x0F, 0x04, 0x070)
set ::REGISTERS(CSR:0x0017C078-3) 0x040803C3 ;# PCI (0x01, 0x0F, 0x04, 0x078)
set ::REGISTERS(CSR:0x0017C080-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x080)
set ::REGISTERS(CSR:0x0017C088-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x088)
set ::REGISTERS(CSR:0x0017C090-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x090)
set ::REGISTERS(CSR:0x0017C098-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x098)
set ::REGISTERS(CSR:0x0017C0A0-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A0)
set ::REGISTERS(CSR:0x0017C0A8-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A8)
set ::REGISTERS(CSR:0x0017C0B0-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B0)
set ::REGISTERS(CSR:0x0017C0B8-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B8)
set ::REGISTERS(CSR:0x0017C0C0-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C0)
set ::REGISTERS(CSR:0x0017C0C8-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C8)
set ::REGISTERS(CSR:0x0017C0D0-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D0)
set ::REGISTERS(CSR:0x0017C0D8-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D8)
set ::REGISTERS(CSR:0x0017C0E0-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E0)
set ::REGISTERS(CSR:0x0017C0E8-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E8)
set ::REGISTERS(CSR:0x0017C0F0-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F0)
set ::REGISTERS(CSR:0x0017C0F8-3) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F8)
set ::REGISTERS(CSR:0x0017C064-3) 0x44004400 ;# PCI (0x01, 0x0F, 0x04, 0x064)
set ::REGISTERS(CSR:0x0017C06C-3) 0x55115511 ;# PCI (0x01, 0x0F, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0017C074-3) 0x66226622 ;# PCI (0x01, 0x0F, 0x04, 0x074)
set ::REGISTERS(CSR:0x0017C07C-3) 0x77337733 ;# PCI (0x01, 0x0F, 0x04, 0x07C)
set ::REGISTERS(CSR:0x0017C084-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x084)
set ::REGISTERS(CSR:0x0017C08C-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x08C)
set ::REGISTERS(CSR:0x0017C094-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x094)
set ::REGISTERS(CSR:0x0017C09C-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x09C)
set ::REGISTERS(CSR:0x0017C0A4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0A4)
set ::REGISTERS(CSR:0x0017C0AC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0AC)
set ::REGISTERS(CSR:0x0017C0B4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0B4)
set ::REGISTERS(CSR:0x0017C0BC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0BC)
set ::REGISTERS(CSR:0x0017C0C4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0C4)
set ::REGISTERS(CSR:0x0017C0CC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0CC)
set ::REGISTERS(CSR:0x0017C0D4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0D4)
set ::REGISTERS(CSR:0x0017C0DC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0DC)
set ::REGISTERS(CSR:0x0017C0E4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0E4)
set ::REGISTERS(CSR:0x0017C0EC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0EC)
set ::REGISTERS(CSR:0x0017C0F4-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0F4)
set ::REGISTERS(CSR:0x0017C0FC-3) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0FC)
set ::REGISTERS(CSR:0x00190080-3) 0x0F308003 ;# PCI (0x01, 0x12, 0x00, 0x080)
set ::REGISTERS(CSR:0x00190084-3) 0x45400B88 ;# PCI (0x01, 0x12, 0x00, 0x084)
set ::REGISTERS(CSR:0x00190040-3) 0x0180F7E4 ;# PCI (0x01, 0x12, 0x00, 0x040)
set ::REGISTERS(CSR:0x00190044-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x044)
set ::REGISTERS(CSR:0x00190048-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x048)
set ::REGISTERS(CSR:0x0019004C-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x04C)
set ::REGISTERS(CSR:0x00190050-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x050)
set ::REGISTERS(CSR:0x00190054-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x054)
set ::REGISTERS(CSR:0x00190058-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x058)
set ::REGISTERS(CSR:0x0019005C-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x05C)
set ::REGISTERS(CSR:0x00190060-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x060)
set ::REGISTERS(CSR:0x00190064-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x064)
set ::REGISTERS(CSR:0x00190068-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x068)
set ::REGISTERS(CSR:0x0019006C-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x00, 0x06C)
set ::REGISTERS(CSR:0x00194080-3) 0x0F308003 ;# PCI (0x01, 0x12, 0x04, 0x080)
set ::REGISTERS(CSR:0x00194084-3) 0x45400B88 ;# PCI (0x01, 0x12, 0x04, 0x084)
set ::REGISTERS(CSR:0x00194040-3) 0x0180F7E4 ;# PCI (0x01, 0x12, 0x04, 0x040)
set ::REGISTERS(CSR:0x00194044-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x044)
set ::REGISTERS(CSR:0x00194048-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x048)
set ::REGISTERS(CSR:0x0019404C-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x04C)
set ::REGISTERS(CSR:0x00194050-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x050)
set ::REGISTERS(CSR:0x00194054-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x054)
set ::REGISTERS(CSR:0x00194058-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x058)
set ::REGISTERS(CSR:0x0019405C-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x05C)
set ::REGISTERS(CSR:0x00194060-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x060)
set ::REGISTERS(CSR:0x00194064-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x064)
set ::REGISTERS(CSR:0x00194068-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x068)
set ::REGISTERS(CSR:0x0019406C-3) 0x0180F000 ;# PCI (0x01, 0x12, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0019807C-3) 0x00013D0D ;# PCI (0x01, 0x13, 0x00, 0x07C)
set ::REGISTERS(CSR:0x00198080-3) 0x0180F7E4 ;# PCI (0x01, 0x13, 0x00, 0x080)
set ::REGISTERS(CSR:0x00198084-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x084)
set ::REGISTERS(CSR:0x00198088-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x088)
set ::REGISTERS(CSR:0x0019808C-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x08C)
set ::REGISTERS(CSR:0x00198090-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x090)
set ::REGISTERS(CSR:0x00198094-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x094)
set ::REGISTERS(CSR:0x00198098-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x098)
set ::REGISTERS(CSR:0x0019809C-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001980A0-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001980A4-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001980A8-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001980AC-3) 0x0180F000 ;# PCI (0x01, 0x13, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x001B007C-3) 0x00013D0D ;# PCI (0x01, 0x16, 0x00, 0x07C)
set ::REGISTERS(CSR:0x001B0080-3) 0x0180F7E4 ;# PCI (0x01, 0x16, 0x00, 0x080)
set ::REGISTERS(CSR:0x001B0084-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x084)
set ::REGISTERS(CSR:0x001B0088-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x088)
set ::REGISTERS(CSR:0x001B008C-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x08C)
set ::REGISTERS(CSR:0x001B0090-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x090)
set ::REGISTERS(CSR:0x001B0094-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x094)
set ::REGISTERS(CSR:0x001B0098-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x098)
set ::REGISTERS(CSR:0x001B009C-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001B00A0-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001B00A4-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001B00A8-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001B00AC-3) 0x0180F000 ;# PCI (0x01, 0x16, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x0017C050-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x050)
set ::REGISTERS(CSR:0x0017C054-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x054)
set ::REGISTERS(CSR:0x0017C058-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x058)
set ::REGISTERS(CSR:0x0017C05C-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x05C)
set ::REGISTERS(CSR:0x000280A8-4) 0xBC000000 ;# PCI (0x00, 0x05, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x000280AC-4) 0xBFF00000 ;# PCI (0x00, 0x05, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x0017D0C0-4) 0xC0000001 ;# PCI (0x01, 0x0F, 0x05, 0x0C0)
set ::REGISTERS(CSR:0x0017D0C4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0C4)
set ::REGISTERS(CSR:0x0017D040-4) 0xDB000069 ;# PCI (0x01, 0x0F, 0x05, 0x040)
set ::REGISTERS(CSR:0x0017D044-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x044)
set ::REGISTERS(CSR:0x0017D048-4) 0xE700006F ;# PCI (0x01, 0x0F, 0x05, 0x048)
set ::REGISTERS(CSR:0x0017D04C-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x04C)
set ::REGISTERS(CSR:0x0017D050-4) 0xF3000075 ;# PCI (0x01, 0x0F, 0x05, 0x050)
set ::REGISTERS(CSR:0x0017D054-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x054)
set ::REGISTERS(CSR:0x0017D058-4) 0xFB00007B ;# PCI (0x01, 0x0F, 0x05, 0x058)
set ::REGISTERS(CSR:0x0017D05C-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x05C)
set ::REGISTERS(CSR:0x0017D060-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x060)
set ::REGISTERS(CSR:0x0017D064-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x064)
set ::REGISTERS(CSR:0x0017D068-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x068)
set ::REGISTERS(CSR:0x0017D06C-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x06C)
set ::REGISTERS(CSR:0x0017D070-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x070)
set ::REGISTERS(CSR:0x0017D074-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x074)
set ::REGISTERS(CSR:0x0017D078-4) 0xFF00007F ;# PCI (0x01, 0x0F, 0x05, 0x078)
set ::REGISTERS(CSR:0x0017D07C-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x07C)
set ::REGISTERS(CSR:0x0017D080-4) 0xFF010001 ;# PCI (0x01, 0x0F, 0x05, 0x080)
set ::REGISTERS(CSR:0x0017D084-4) 0x0000023F ;# PCI (0x01, 0x0F, 0x05, 0x084)
set ::REGISTERS(CSR:0x0017D088-4) 0xFF012001 ;# PCI (0x01, 0x0F, 0x05, 0x088)
set ::REGISTERS(CSR:0x0017D08C-4) 0x0000027F ;# PCI (0x01, 0x0F, 0x05, 0x08C)
set ::REGISTERS(CSR:0x0017D090-4) 0xFF014001 ;# PCI (0x01, 0x0F, 0x05, 0x090)
set ::REGISTERS(CSR:0x0017D094-4) 0x000002BF ;# PCI (0x01, 0x0F, 0x05, 0x094)
set ::REGISTERS(CSR:0x0017D098-4) 0xFF016001 ;# PCI (0x01, 0x0F, 0x05, 0x098)
set ::REGISTERS(CSR:0x0017D09C-4) 0x000002FF ;# PCI (0x01, 0x0F, 0x05, 0x09C)
set ::REGISTERS(CSR:0x0017D0A0-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A0)
set ::REGISTERS(CSR:0x0017D0A4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A4)
set ::REGISTERS(CSR:0x0017D0A8-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0A8)
set ::REGISTERS(CSR:0x0017D0AC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0AC)
set ::REGISTERS(CSR:0x0017D0B0-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B0)
set ::REGISTERS(CSR:0x0017D0B4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B4)
set ::REGISTERS(CSR:0x0017D0B8-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0B8)
set ::REGISTERS(CSR:0x0017D0BC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x05, 0x0BC)
set ::REGISTERS(CSR:0x0017C060-4) 0x040203C3 ;# PCI (0x01, 0x0F, 0x04, 0x060)
set ::REGISTERS(CSR:0x0017C068-4) 0x040403C3 ;# PCI (0x01, 0x0F, 0x04, 0x068)
set ::REGISTERS(CSR:0x0017C070-4) 0x040603C3 ;# PCI (0x01, 0x0F, 0x04, 0x070)
set ::REGISTERS(CSR:0x0017C078-4) 0x040803C3 ;# PCI (0x01, 0x0F, 0x04, 0x078)
set ::REGISTERS(CSR:0x0017C080-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x080)
set ::REGISTERS(CSR:0x0017C088-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x088)
set ::REGISTERS(CSR:0x0017C090-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x090)
set ::REGISTERS(CSR:0x0017C098-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x098)
set ::REGISTERS(CSR:0x0017C0A0-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A0)
set ::REGISTERS(CSR:0x0017C0A8-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0A8)
set ::REGISTERS(CSR:0x0017C0B0-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B0)
set ::REGISTERS(CSR:0x0017C0B8-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0B8)
set ::REGISTERS(CSR:0x0017C0C0-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C0)
set ::REGISTERS(CSR:0x0017C0C8-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0C8)
set ::REGISTERS(CSR:0x0017C0D0-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D0)
set ::REGISTERS(CSR:0x0017C0D8-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0D8)
set ::REGISTERS(CSR:0x0017C0E0-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E0)
set ::REGISTERS(CSR:0x0017C0E8-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0E8)
set ::REGISTERS(CSR:0x0017C0F0-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F0)
set ::REGISTERS(CSR:0x0017C0F8-4) 0x040803C2 ;# PCI (0x01, 0x0F, 0x04, 0x0F8)
set ::REGISTERS(CSR:0x0017C064-4) 0x44004400 ;# PCI (0x01, 0x0F, 0x04, 0x064)
set ::REGISTERS(CSR:0x0017C06C-4) 0x55115511 ;# PCI (0x01, 0x0F, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0017C074-4) 0x66226622 ;# PCI (0x01, 0x0F, 0x04, 0x074)
set ::REGISTERS(CSR:0x0017C07C-4) 0x77337733 ;# PCI (0x01, 0x0F, 0x04, 0x07C)
set ::REGISTERS(CSR:0x0017C084-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x084)
set ::REGISTERS(CSR:0x0017C08C-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x08C)
set ::REGISTERS(CSR:0x0017C094-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x094)
set ::REGISTERS(CSR:0x0017C09C-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x09C)
set ::REGISTERS(CSR:0x0017C0A4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0A4)
set ::REGISTERS(CSR:0x0017C0AC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0AC)
set ::REGISTERS(CSR:0x0017C0B4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0B4)
set ::REGISTERS(CSR:0x0017C0BC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0BC)
set ::REGISTERS(CSR:0x0017C0C4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0C4)
set ::REGISTERS(CSR:0x0017C0CC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0CC)
set ::REGISTERS(CSR:0x0017C0D4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0D4)
set ::REGISTERS(CSR:0x0017C0DC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0DC)
set ::REGISTERS(CSR:0x0017C0E4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0E4)
set ::REGISTERS(CSR:0x0017C0EC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0EC)
set ::REGISTERS(CSR:0x0017C0F4-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0F4)
set ::REGISTERS(CSR:0x0017C0FC-4) 0x00000000 ;# PCI (0x01, 0x0F, 0x04, 0x0FC)
set ::REGISTERS(CSR:0x00190080-4) 0x0F308003 ;# PCI (0x01, 0x12, 0x00, 0x080)
set ::REGISTERS(CSR:0x00190084-4) 0x45400B88 ;# PCI (0x01, 0x12, 0x00, 0x084)
set ::REGISTERS(CSR:0x00190040-4) 0x0200F7E4 ;# PCI (0x01, 0x12, 0x00, 0x040)
set ::REGISTERS(CSR:0x00190044-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x044)
set ::REGISTERS(CSR:0x00190048-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x048)
set ::REGISTERS(CSR:0x0019004C-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x04C)
set ::REGISTERS(CSR:0x00190050-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x050)
set ::REGISTERS(CSR:0x00190054-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x054)
set ::REGISTERS(CSR:0x00190058-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x058)
set ::REGISTERS(CSR:0x0019005C-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x05C)
set ::REGISTERS(CSR:0x00190060-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x060)
set ::REGISTERS(CSR:0x00190064-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x064)
set ::REGISTERS(CSR:0x00190068-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x068)
set ::REGISTERS(CSR:0x0019006C-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x00, 0x06C)
set ::REGISTERS(CSR:0x00194080-4) 0x0F308003 ;# PCI (0x01, 0x12, 0x04, 0x080)
set ::REGISTERS(CSR:0x00194084-4) 0x45400B88 ;# PCI (0x01, 0x12, 0x04, 0x084)
set ::REGISTERS(CSR:0x00194040-4) 0x0200F7E4 ;# PCI (0x01, 0x12, 0x04, 0x040)
set ::REGISTERS(CSR:0x00194044-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x044)
set ::REGISTERS(CSR:0x00194048-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x048)
set ::REGISTERS(CSR:0x0019404C-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x04C)
set ::REGISTERS(CSR:0x00194050-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x050)
set ::REGISTERS(CSR:0x00194054-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x054)
set ::REGISTERS(CSR:0x00194058-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x058)
set ::REGISTERS(CSR:0x0019405C-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x05C)
set ::REGISTERS(CSR:0x00194060-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x060)
set ::REGISTERS(CSR:0x00194064-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x064)
set ::REGISTERS(CSR:0x00194068-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x068)
set ::REGISTERS(CSR:0x0019406C-4) 0x0200F000 ;# PCI (0x01, 0x12, 0x04, 0x06C)
set ::REGISTERS(CSR:0x0019807C-4) 0x00013D0D ;# PCI (0x01, 0x13, 0x00, 0x07C)
set ::REGISTERS(CSR:0x00198080-4) 0x0200F7E4 ;# PCI (0x01, 0x13, 0x00, 0x080)
set ::REGISTERS(CSR:0x00198084-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x084)
set ::REGISTERS(CSR:0x00198088-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x088)
set ::REGISTERS(CSR:0x0019808C-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x08C)
set ::REGISTERS(CSR:0x00198090-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x090)
set ::REGISTERS(CSR:0x00198094-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x094)
set ::REGISTERS(CSR:0x00198098-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x098)
set ::REGISTERS(CSR:0x0019809C-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001980A0-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001980A4-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001980A8-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001980AC-4) 0x0200F000 ;# PCI (0x01, 0x13, 0x00, 0x0AC)
set ::REGISTERS(CSR:0x001B007C-4) 0x00013D0D ;# PCI (0x01, 0x16, 0x00, 0x07C)
set ::REGISTERS(CSR:0x001B0080-4) 0x0200F7E4 ;# PCI (0x01, 0x16, 0x00, 0x080)
set ::REGISTERS(CSR:0x001B0084-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x084)
set ::REGISTERS(CSR:0x001B0088-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x088)
set ::REGISTERS(CSR:0x001B008C-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x08C)
set ::REGISTERS(CSR:0x001B0090-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x090)
set ::REGISTERS(CSR:0x001B0094-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x094)
set ::REGISTERS(CSR:0x001B0098-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x098)
set ::REGISTERS(CSR:0x001B009C-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x09C)
set ::REGISTERS(CSR:0x001B00A0-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x0A0)
set ::REGISTERS(CSR:0x001B00A4-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x0A4)
set ::REGISTERS(CSR:0x001B00A8-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x0A8)
set ::REGISTERS(CSR:0x001B00AC-4) 0x0200F000 ;# PCI (0x01, 0x16, 0x00, 0x0AC)
set ::CpuMap(1) 1
set ::CpuMap(2) 2
set ::CpuMap(3) 3
set ::CpuMap(4) 4
set ::PLATFORM    ANDROMEDA
set ::CPUTYPE    CPU_HSX
main
