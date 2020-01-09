#include "ei.h"



int main() {
    ei_init();
    {
        ei_x_buff buf;
        ei_x_new(&buf);
        int i = 0;
        ei_x_encode_tuple_header(&buf, 2);
        ei_x_encode_atom(&buf, "tobbe");
        ei_x_encode_long(&buf, 3928);

        printf("HEJ\n");
        ei_print_term(stdout, buf.buff, &i);
        printf("\nHEJ\n");
    }
    {
        int i = 0;
        ei_x_buff buf;
        ei_x_new(&buf);
        ei_x_format_wo_ver(&buf, "{~a,~i}", "tobbe", 3928);
        printf("HEJ2\n");
        ei_print_term(stdout, buf.buff, &i);
        printf("\nHEJ2\n");
    }
    {
        int i = 0;
        ei_x_buff buf;
        ei_x_new(&buf);
        ei_x_format_wo_ver(&buf,
                           "[{name,~a},{age,~i},{data,[{adr,~s,~i}]}]",
                           "madonna",
                           21,
                           "E-street", 42);
        printf("HEJ3\n");
        ei_print_term(stdout, buf.buff, &i);
        printf("\nHEJ3\n");
        ei_x_free(&buf);
    }
}
