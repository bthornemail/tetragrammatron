#!/usr/bin/env sh
# genesis-enhanced.sh — GENESIS with customizable P₀/P₁ symbols
# Enhancement: Support for universal Alpha/Omega constants
set -eu

# Default symbols (can be overridden by environment variables)
: "${GENESIS_P0_SYMBOL:=A}"      # Alphanumeric (semantic mass) - Alpha/Light
: "${GENESIS_P1_SYMBOL:=S}"      # Separator (structure) - Omega/Dark
: "${GENESIS_WS_SYMBOL:=_}"      # Whitespace

# For Tetragrammatron, you can set:
# export GENESIS_P0_SYMBOL="α"  # Greek alpha
# export GENESIS_P1_SYMBOL="Ω"  # Greek omega
# export GENESIS_WS_SYMBOL="∘"  # Composition operator
#
# Or use emoji/unicode:
# export GENESIS_P0_SYMBOL="🜂"  # Alchemical light
# export GENESIS_P1_SYMBOL="🜃"  # Alchemical dark
#
# Or octonionic:
# export GENESIS_P0_SYMBOL="e₀"  # Real unit
# export GENESIS_P1_SYMBOL="∑"   # Imaginary sum

HARDWARE_RE='^([0-9A-Fa-f]{2}:)*[0-9A-Fa-f]{2}$'

die(){ printf "genesis: %s\n" "$*" >&2; exit 1; }
have(){ command -v "$1" >/dev/null 2>&1; }

now_iso(){
  if have date; then date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date; else echo "unknown-time"; fi
}

stat_fields(){
  f="$1"
  if stat --version >/dev/null 2>&1; then
    stat -c "%d %i %f %u %g %s %X %Y %Z" "$f"
    return
  fi
  if stat -f "%d %i %p %u %g %z %a %m %c" "$f" >/dev/null 2>&1; then
    stat -f "%d %i %p %u %g %z %a %m %c" "$f"
    return
  fi
  sz="$(wc -c <"$f" 2>/dev/null || echo 0)"
  printf "0 0 0 0 0 %s 0 0 0\n" "$sz"
}

atom_id_from_string(){
  printf "%s" "$1" | tr -cd '[:alnum:]' | tr '[:lower:]' '[:upper:]'
}

hex2(){ awk -v n="$1" 'BEGIN{printf "%02X",(n%256+256)%256}'; }

hardware_probe(){
  f="$1"
  set -- $(stat_fields "$f")
  dev="$1"; ino="$2"; mode="$3"; uid="$4"; gid="$5"; size="$6"; mt="$8"; ct="$9"
  b0="$(hex2 "$ino")"; b1="$(hex2 "$dev")"; b2="$(hex2 "$mode")"; b3="$(hex2 "$uid")"
  b4="$(hex2 "$gid")"; b5="$(hex2 "$size")"; b6="$(hex2 "$mt")"; b7="$(hex2 "$ct")"
  printf "%s:%s:%s:%s:%s:%s:%s:%s" "$b0" "$b1" "$b2" "$b3" "$b4" "$b5" "$b6" "$b7"
}

software_probe8(){
  f="$1"
  awk '
    BEGIN{total=0; aruns=0; nruns=0; maxa=0; maxn=0; trans=0; newlines=0; ina=0; len=0; first_set=0; first_non=0}
    {
      line=$0 "\n";
      for(i=1;i<=length(line);i++){
        c=substr(line,i,1);
        if(first_set==0){ first_set=1; if(c !~ /[[:alnum:]]/) first_non=1; }
        total++;
        if(c=="\n"){ newlines++; }
        isalnum=(c ~ /[[:alnum:]]/);
        if(isalnum){
          if(ina==0){ aruns++; if(total>1) trans++; ina=1; len=1; } else { len++; }
          if(len>maxa) maxa=len;
        } else {
          if(ina==1){ nruns++; trans++; ina=0; len=1; } else { len++; }
          if(len>maxn) maxn=len;
          if(c !~ /[[:space:]]/){ sep[c]=1; }
        }
      }
    }
    END{
      s=0; for(k in sep) s++;
      if(first_set==1 && first_non==1){ nruns=(nruns+1); }
      printf "%d %d %d %d %d %d %d %d\n",
        (total%256),(aruns%256),(nruns%256),(maxa%256),(maxn%256),(trans%256),(s%256),(newlines%256);
    }
  ' "$f"
}

software_form_probe(){
  # Enhanced: Uses customizable symbols from environment
  f="$1"
  p0_sym="${GENESIS_P0_SYMBOL}"
  p1_sym="${GENESIS_P1_SYMBOL}"
  ws_sym="${GENESIS_WS_SYMBOL}"

  awk -v P0="$p0_sym" -v P1="$p1_sym" -v WS="$ws_sym" '
    {
      line=$0 "\n";
      for(i=1;i<=length(line);i++){
        c=substr(line,i,1);
        t=P1;
        if(c ~ /[[:alnum:]]/) t=P0;
        else if(c ~ /[[:space:]]/) t=WS;
        printf "%s",t;
      }
    }
  ' "$f"
}

mux8(){
  f="$1"
  awk '
    BEGIN{id=0;inc=0;ign=0;sch=0;scm=0;ctx=0;bnd=0;col=0;prev="";depth=0}
    {
      line=$0 "\n";
      for(i=1;i<=length(line);i++){
        c=substr(line,i,1);
        t="S";
        if(c ~ /[[:alnum:]]/) t="A";
        else if(c ~ /[[:space:]]/) t="_";
        id++;
        if(prev!="" && prev!=t) inc++;
        else if(prev==t && t!="A") ign++;
        if(c=="("||c=="["||c=="{") {sch++; depth++;}
        if(c==")"||c=="]"||c=="}") {scm++; if(depth>0) depth--;}
        if(c=="\"" || c=="\047") ctx++;
        if(t=="S") bnd++;
        if(c=="\n") col++;
        prev=t;
      }
    }
    END{printf "%d %d %d %d %d %d %d %d\n", (id%256),(inc%256),(ign%256),(sch%256),(scm%256),(ctx%256),(bnd%256),(col%256);}
  ' "$f"
}

cmd_translate(){
  f="$1"
  [ -f "$f" ] || die "not a file: $f"
  hw="$(hardware_probe "$f")"
  v="$(software_probe8 "$f" | tr ' ' ',')"
  form="$(software_form_probe "$f")"

  # Output with symbol legend
  printf "HW=%s V8=%s\n" "$hw" "$v"
  printf "FORM=%s\n" "$form"
  printf "# Legend: %s=P₀(alphanumeric) %s=P₁(structure) %s=whitespace\n" \
    "$GENESIS_P0_SYMBOL" "$GENESIS_P1_SYMBOL" "$GENESIS_WS_SYMBOL"
}

cmd_mux8(){
  f="$1"
  [ -f "$f" ] || die "not a file: $f"
  mux8 "$f"
}

case "${1:-}" in
  translate) cmd_translate "${2:?missing FILE}" ;;
  mux8) cmd_mux8 "${2:?missing FILE}" ;;
  *)
    cat <<EOF
genesis-enhanced.sh — GENESIS with customizable symbols

USAGE
  genesis-enhanced.sh translate FILE
  genesis-enhanced.sh mux8 FILE

SYMBOLS (customize via environment)
  GENESIS_P0_SYMBOL=${GENESIS_P0_SYMBOL}  (alphanumeric/semantic mass)
  GENESIS_P1_SYMBOL=${GENESIS_P1_SYMBOL}  (structure/separators)
  GENESIS_WS_SYMBOL=${GENESIS_WS_SYMBOL}  (whitespace)

EXAMPLES
  # Default (ASCII)
  ./genesis-enhanced.sh translate file.c

  # Greek Alpha/Omega
  GENESIS_P0_SYMBOL=α GENESIS_P1_SYMBOL=Ω ./genesis-enhanced.sh translate file.c

  # Tetragrammatron Octonionic
  GENESIS_P0_SYMBOL=e₀ GENESIS_P1_SYMBOL=∑ ./genesis-enhanced.sh translate file.canb

  # Alchemical Light/Dark
  GENESIS_P0_SYMBOL=🜂 GENESIS_P1_SYMBOL=🜃 ./genesis-enhanced.sh translate file.canasm

PHILOSOPHY
  Symbols don't matter. Only shape matters.
  Two files with different P₀/P₁ symbols but same alternation pattern are
  structurally equivalent.

EOF
    ;;
esac
