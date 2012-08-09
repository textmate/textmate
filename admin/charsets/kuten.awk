/^[0-9]/ {
  ku=substr($1, 3, 2) + 32;
  ten=substr($1, 5, 2) + 32;
  printf "0x%02X%02X %s\n", ku, ten, $2;
}

