// Kayra Aslan Oncul 241ADB119
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: <Claude>

#define _POSIX_C_SOURCE 200809L

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define STU_NAME     "kayra_aslan"
#define STU_LASTNAME "oncul"
#define STU_ID       "241adb119"

// A value keeps its type, so each operator can pick integer or float arithmetic from its operands.
typedef struct {
    int is_float;
    long long i;
    double d;
} Value;

static double as_double(Value v) { return v.is_float ? v.d : (double)v.i; }

static Value make_int(long long i)
{
    Value v;
    v.is_float = 0;
    v.i = i;
    v.d = 0.0;
    return v;
}

enum { T_NUM, T_PLUS, T_MINUS, T_STAR, T_SLASH, T_POW, T_LPAR, T_RPAR, T_EOL, T_BAD };

typedef struct {
    int type;
    size_t pos;  
    Value val;
} Token;

// The whole file is one buffer and each token stores its offset, so positions count every character incl. '\n' and comments.
static char *src;
static size_t src_len;
static size_t scan;
static Token tok;
static size_t err_pos;   

static void lex_number(void)
{
    size_t start = scan;
    unsigned long long n = 0;

    while (scan < src_len && isdigit((unsigned char)src[scan])) {
        n = n * 10 + (unsigned)(src[scan] - '0');
        scan++;
    }
    tok.type = T_NUM;
    tok.val = make_int((long long)n);

    // A float needs digits on both sides of '.'; otherwise the '.' (or 'e') becomes T_BAD at its own position.
    if (scan + 1 < src_len && src[scan] == '.' && isdigit((unsigned char)src[scan + 1])) {
        char saved;
        scan++;
        while (scan < src_len && isdigit((unsigned char)src[scan]))
            scan++;
        saved = src[scan];   
        src[scan] = '\0';
        tok.val.is_float = 1;
        tok.val.i = 0;
        tok.val.d = strtod(src + start, NULL);
        src[scan] = saved;
        if (!isfinite(tok.val.d))
            tok.type = T_BAD;
    }
}

static void advance(void)
{
    char c;

    while (scan < src_len && (src[scan] == ' ' || src[scan] == '\t'))
        scan++;
    tok.pos = scan;
    if (scan >= src_len || src[scan] == '\n') {
        tok.type = T_EOL;
        return;
    }
    c = src[scan];
    if (isdigit((unsigned char)c)) {
        lex_number();
        return;
    }
    scan++;
    switch (c) {
    case '+': tok.type = T_PLUS;  break;
    case '-': tok.type = T_MINUS; break;
    case '/': tok.type = T_SLASH; break;
    case '(': tok.type = T_LPAR;  break;
    case ')': tok.type = T_RPAR;  break;
    case '*':
        if (scan < src_len && src[scan] == '*') {  
            scan++;
            tok.type = T_POW;
        } else {
            tok.type = T_STAR;
        }
        break;
    default:
        tok.type = T_BAD;
        break;
    }
}

// Infinite or NaN float results are reported at the operator.
static Value make_float(double d, size_t op_pos)
{
    Value v;
    v.is_float = 1;
    v.i = 0;
    v.d = d;
    if (!isfinite(d))
        err_pos = op_pos + 1;
    return v;
}

// Both integers: long long arithmetic (unsigned to avoid overflow UB); if either is a float: double arithmetic.
static Value arith(int op, Value a, Value b, size_t op_pos, size_t rhs_pos)
{
    if (!a.is_float && !b.is_float) {
        unsigned long long x = (unsigned long long)a.i;
        unsigned long long y = (unsigned long long)b.i;
        switch (op) {
        case T_PLUS:  return make_int((long long)(x + y));
        case T_MINUS: return make_int((long long)(x - y));
        case T_STAR:  return make_int((long long)(x * y));
        default:
            if (b.i == 0) { 
                err_pos = rhs_pos + 1;
                return make_int(0);
            }
            if (b.i == -1)
                return make_int((long long)(0ULL - x));
            return make_int(a.i / b.i);  
        }
    } else {
        double x = as_double(a);
        double y = as_double(b);
        switch (op) {
        case T_PLUS:  return make_float(x + y, op_pos);
        case T_MINUS: return make_float(x - y, op_pos);
        case T_STAR:  return make_float(x * y, op_pos);
        default:
            if (y == 0.0) {
                err_pos = rhs_pos + 1;
                return make_int(0);
            }
            return make_float(x / y, op_pos);
        }
    }
}

static Value power_op(Value a, Value b, size_t op_pos, size_t exp_pos)
{
    if (!a.is_float && !b.is_float) {
        unsigned long long result = 1;
        unsigned long long base = (unsigned long long)a.i;
        long long e = b.i;

        if (e < 0) {   
            err_pos = exp_pos + 1;
            return make_int(0);
        }
        while (e > 0) {   
            if (e & 1)
                result *= base;
            e >>= 1;
            if (e)
                base *= base;
        }
        return make_int((long long)result);
    }
    return make_float(pow(as_double(a), as_double(b)), op_pos);
}

static Value parse_expr(void);

// Any token that cannot continue the expression is reported at its own position.
static Value parse_primary(void)
{
    Value v = make_int(0);

    if (tok.type == T_NUM) {
        v = tok.val;
        advance();
        return v;
    }
    if (tok.type == T_LPAR) {
        advance();
        v = parse_expr();
        if (err_pos)
            return v;
        if (tok.type != T_RPAR) {   
            err_pos = tok.pos + 1;
            return v;
        }
        advance();
        return v;
    }
    err_pos = tok.pos + 1;
    return v;
}

static Value parse_power(void)
{
    Value base = parse_primary();

    if (err_pos)
        return base;
    if (tok.type == T_POW) {
        size_t op_pos = tok.pos;
        size_t exp_pos;
        Value e;

        advance();
        exp_pos = tok.pos;
        e = parse_power();
        if (err_pos)
            return e;
        base = power_op(base, e, op_pos, exp_pos);
    }
    return base;
}

static Value parse_term(void)
{
    Value left = parse_power();

    while (!err_pos && (tok.type == T_STAR || tok.type == T_SLASH)) {
        int op = tok.type;
        size_t op_pos = tok.pos;
        size_t rhs_pos;
        Value right;

        advance();
        rhs_pos = tok.pos;
        right = parse_power();
        if (err_pos)
            break;
        left = arith(op, left, right, op_pos, rhs_pos);
    }
    return left;
}

static Value parse_expr(void)
{
    Value left = parse_term();

    while (!err_pos && (tok.type == T_PLUS || tok.type == T_MINUS)) {
        int op = tok.type;
        size_t op_pos = tok.pos;
        Value right;

        advance();
        right = parse_term();
        if (err_pos)
            break;
        left = arith(op, left, right, op_pos, 0);
    }
    return left;
}

static void *xmalloc(size_t n)
{
    void *p = malloc(n);
    if (!p) {
        fprintf(stderr, "calc: out of memory\n");
        exit(1);
    }
    return p;
}

static void *xrealloc(void *old, size_t n)
{
    void *p = realloc(old, n);
    if (!p) {
        fprintf(stderr, "calc: out of memory\n");
        exit(1);
    }
    return p;
}

// Output buffer whose capacity doubles, so large files are not slowed down by repeated copying.
typedef struct {
    char *data;
    size_t len, cap;
} Buf;

static void buf_init(Buf *b)
{
    b->cap = 4096;
    b->len = 0;
    b->data = xmalloc(b->cap);
}

static void buf_add(Buf *b, const char *s, size_t n)
{
    if (b->len + n > b->cap) {
        while (b->len + n > b->cap)
            b->cap *= 2;
        b->data = xrealloc(b->data, b->cap);
    }
    memcpy(b->data + b->len, s, n);
    b->len += n;
}

// Results are kept in memory until the file is done, because one error means the output is only that error.
static char *evaluate(char *text, size_t len, size_t *out_len)
{
    Buf out;
    size_t p = 0;

    buf_init(&out);
    src = text;
    src_len = len;

    while (p < len) {
        size_t q = p;
        Value v;
        char tmp[64];
        int n;

        while (q < len && (text[q] == ' ' || text[q] == '\t'))
            q++;
        if (q >= len)
            break;
        if (text[q] == '\n') {   
            p = q + 1;
            continue;
        }
        if (text[q] == '#') {  
            while (q < len && text[q] != '\n')
                q++;
            p = q + 1;
            continue;
        }

        scan = p;
        err_pos = 0;
        advance();
        v = parse_expr();
        if (!err_pos && tok.type != T_EOL)  
            err_pos = tok.pos + 1;
        if (err_pos) {
            n = snprintf(tmp, sizeof tmp, "ERROR:%zu\n", err_pos);
            out.len = 0;   
            buf_add(&out, tmp, (size_t)n);
            break;
        }

        if (v.is_float)
            n = snprintf(tmp, sizeof tmp, "%.15g\n", v.d);
        else
            n = snprintf(tmp, sizeof tmp, "%lld\n", v.i);
        buf_add(&out, tmp, (size_t)n);

        while (scan < len && text[scan] != '\n')
            scan++;
        if (scan < len && text[scan] == '\n')
            scan++;
        p = scan;
    }

    *out_len = out.len;
    return out.data;
}

// Reads the whole file at once.
static char *read_file(const char *path, size_t *len)
{
    FILE *f = fopen(path, "rb");
    size_t cap = 1 << 16, n = 0, got;
    char *data;

    if (!f)
        return NULL;
    data = xmalloc(cap + 1);
    while ((got = fread(data + n, 1, cap - n, f)) > 0) {
        n += got;
        if (n == cap) {
            cap *= 2;
            data = xrealloc(data, cap + 1);
        }
    }
    if (ferror(f)) {
        int e = errno;
        free(data);
        fclose(f);
        errno = e;
        return NULL;
    }
    fclose(f);
    data[n] = '\0';
    *len = n;
    return data;
}

static char *path_join(const char *a, const char *b)
{
    size_t la = strlen(a), lb = strlen(b);
    char *r = xmalloc(la + lb + 2);

    memcpy(r, a, la);
    r[la] = '/';
    memcpy(r + la + 1, b, lb);
    r[la + 1 + lb] = '\0';
    return r;
}

// Last path component.
static char *base_name(const char *path, int strip_txt)
{
    size_t len = strlen(path);
    const char *start;
    char *r;

    while (len > 1 && path[len - 1] == '/')
        len--;
    start = path;
    for (size_t i = 0; i < len; i++)
        if (path[i] == '/')
            start = path + i + 1;
    len -= (size_t)(start - path);
    if (strip_txt && len > 4 && strncmp(start + len - 4, ".txt", 4) == 0)
        len -= 4;
    r = xmalloc(len + 1);
    memcpy(r, start, len);
    r[len] = '\0';
    return r;
}

static int outdir_ready;

static int ensure_outdir(const char *path)
{
    if (outdir_ready)
        return 0;
    if (mkdir(path, 0777) != 0) {
        int e = errno;
        struct stat st;

        if (e != EEXIST || stat(path, &st) != 0 || !S_ISDIR(st.st_mode)) {
            fprintf(stderr, "calc: cannot create output folder '%s': %s\n", path,
                    e == EEXIST ? "a file with that name exists" : strerror(e));
            return -1;
        }
    }
    outdir_ready = 1;
    return 0;
}

static int process_file(const char *in_path, const char *base, const char *outdir)
{
    size_t len, out_len;
    char *text, *result, *out_path, *file_name;
    FILE *f;
    int ok;

    text = read_file(in_path, &len);
    if (!text) {
        fprintf(stderr, "calc: cannot read '%s': %s\n", in_path, strerror(errno));
        return -1;
    }
    if (ensure_outdir(outdir) != 0) {
        free(text);
        return -1;
    }

    result = evaluate(text, len, &out_len);

    file_name = xmalloc(strlen(base) + strlen(STU_NAME) + strlen(STU_LASTNAME)
                        + strlen(STU_ID) + 8);
    sprintf(file_name, "%s_%s_%s_%s.txt", base, STU_NAME, STU_LASTNAME, STU_ID);
    out_path = path_join(outdir, file_name);

    f = fopen(out_path, "wb");
    ok = f != NULL;
    if (ok) {
        ok = fwrite(result, 1, out_len, f) == out_len;
        if (fclose(f) != 0)
            ok = 0;
    }
    if (!ok)
        fprintf(stderr, "calc: cannot write '%s': %s\n", out_path, strerror(errno));

    free(text);
    free(result);
    free(file_name);
    free(out_path);
    return ok ? 0 : -1;
}

static int cmp_names(const void *a, const void *b)
{
    return strcmp(*(char *const *)a, *(char *const *)b);
}

static int usage(const char *msg)
{
    if (msg)
        fprintf(stderr, "calc: %s\n", msg);
    fprintf(stderr, "usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
    return 1;
}

int main(int argc, char **argv)
{
    const char *dir = NULL, *outdir = NULL, *input = NULL;
    char *default_out = NULL, *base;
    int rc = 0;

    for (int i = 1; i < argc; i++) {
        const char *a = argv[i];

        if (strcmp(a, "-d") == 0 || strcmp(a, "--dir") == 0) {
            if (++i >= argc)
                return usage("option needs an argument");
            dir = argv[i];
        } else if (strcmp(a, "-o") == 0 || strcmp(a, "--output-dir") == 0) {
            if (++i >= argc)
                return usage("option needs an argument");
            outdir = argv[i];
        } else if (a[0] == '-' && a[1] != '\0') {
            fprintf(stderr, "calc: unknown option '%s'\n", a);
            return usage(NULL);
        } else if (input) {
            return usage("only one input file is allowed");
        } else {
            input = a;
        }
    }
    if (!dir && !input)
        return usage("no input given");
    if (dir && input)
        return usage("use either -d DIR or an input file, not both");

    if (dir) {
        DIR *d = opendir(dir);
        struct dirent *ent;
        char **names = NULL;
        size_t count = 0, cap = 0;

        if (!d) {
            fprintf(stderr, "calc: cannot open folder '%s': %s\n", dir, strerror(errno));
            return 1;
        }
        // Collect names first so result files written into DIR itself are not read as input.
        while ((ent = readdir(d)) != NULL) {
            size_t n = strlen(ent->d_name);
            char *full;
            struct stat st;

            if (n <= 4 || strcmp(ent->d_name + n - 4, ".txt") != 0)  
                continue;
            full = path_join(dir, ent->d_name);
            if (stat(full, &st) != 0 || !S_ISREG(st.st_mode)) {
                free(full);
                continue;
            }
            free(full);
            if (count == cap) {
                cap = cap ? cap * 2 : 16;
                names = xrealloc(names, cap * sizeof *names);
            }
            names[count++] = strdup(ent->d_name);
            if (!names[count - 1]) {
                fprintf(stderr, "calc: out of memory\n");
                return 1;
            }
        }
        closedir(d);
        qsort(names, count, sizeof *names, cmp_names);

        if (!outdir) {   
            base = base_name(dir, 0);
            default_out = xmalloc(strlen(base) + strlen(STU_NAME) + strlen(STU_ID) + 3);
            sprintf(default_out, "%s_%s_%s", base, STU_NAME, STU_ID);
            free(base);
            outdir = default_out;
        }
        if (ensure_outdir(outdir) != 0)
            rc = 1;
        for (size_t i = 0; i < count && rc == 0; i++) {
            char *full = path_join(dir, names[i]);
            char *file_base = base_name(names[i], 1);

            if (process_file(full, file_base, outdir) != 0)
                rc = 1;
            free(full);
            free(file_base);
        }
        for (size_t i = 0; i < count; i++)
            free(names[i]);
        free(names);
    } else {
        base = base_name(input, 1);
        if (!outdir) {   
            default_out = xmalloc(strlen(base) + strlen(STU_NAME) + strlen(STU_ID) + 3);
            sprintf(default_out, "%s_%s_%s", base, STU_NAME, STU_ID);
            outdir = default_out;
        }
        if (process_file(input, base, outdir) != 0)
            rc = 1;
        free(base);
    }
    free(default_out);
    return rc;
}
