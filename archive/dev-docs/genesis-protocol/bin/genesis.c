// genesis.c - minimal Layer 0 atom emitter (POSIX)
// Build: cc -O2 -Wall -o genesis genesis.c
// Usage: ./genesis atom PATH
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

static void json_escape(const char *in, FILE *out) {
  for (const unsigned char *p=(const unsigned char*)in; *p; p++) {
    switch (*p) {
      case '\\': fputs("\\\\", out); break;
      case '"':  fputs("\\\"", out); break;
      case '\n': fputs("\\n", out); break;
      case '\r': fputs("\\r", out); break;
      case '\t': fputs("\\t", out); break;
      default:
        if (*p < 0x20) fprintf(out, "\\u%04x", *p);
        else fputc(*p, out);
    }
  }
}

static void now_iso_utc(char *buf, size_t n) {
  time_t t = time(NULL);
  struct tm tm;
  gmtime_r(&t, &tm);
  strftime(buf, n, "%Y-%m-%dT%H:%M:%SZ", &tm);
}

static int atom(const char *path) {
  struct stat st;
  if (lstat(path, &st) != 0) return 2;

  char ts[32]; now_iso_utc(ts, sizeof(ts));

  printf("{\"t\":\"%s\",\"k\":\"genesis.atom\",\"v\":{\"path\":\"", ts);
  json_escape(path, stdout);
  printf("\",\"stat\":{");
  printf("\"dev\":%llu,", (unsigned long long)st.st_dev);
  printf("\"ino\":%llu,", (unsigned long long)st.st_ino);
  printf("\"mode\":%u,", (unsigned)st.st_mode);
  printf("\"uid\":%u,", (unsigned)st.st_uid);
  printf("\"gid\":%u,", (unsigned)st.st_gid);
  printf("\"size\":%lld,", (long long)st.st_size);
  printf("\"atime\":%lld,", (long long)st.st_atime);
  printf("\"mtime\":%lld,", (long long)st.st_mtime);
  printf("\"ctime\":%lld", (long long)st.st_ctime);
  printf("}}}\n");
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s atom PATH\n", argv[0]);
    return 1;
  }
  if (strcmp(argv[1], "atom") == 0) {
    if (argc < 3) { fprintf(stderr, "missing PATH\n"); return 1; }
    return atom(argv[2]);
  }
  fprintf(stderr, "unknown command: %s\n", argv[1]);
  return 1;
}
