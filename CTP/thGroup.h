struct thGroupOpaque {		/* Opaque structure for group definitions */
  daVarStructList *blocklist;
  char *type;			/* i.e. hist, test, gethit, ... */
  thStatus (*book)();		/* Hooks to the appropriate routines */
  thStatus (*execute)();
  thStatus (*clear)();
  thStatus (*clearScalers)();
  thStatus (*incrementScalers)();
};
typedef struct thGroupOpaque thGroupOpaque;

void thInitGroupOpaque(char *name, thGroupOpaque *opqptr);
/*void thInitGroupOpaque(char *name, (thGroupOpaque *) opqptr);*/


