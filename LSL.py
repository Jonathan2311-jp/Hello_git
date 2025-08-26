class Nodo:
    def __init__(self,dato):
        self.dato=dato
        self.siguiente=None

class LSL:
    def __init__(self):
        self.primero=None

# // Insertar al incio   
    def insertar(self,dato):
        nodo=Nodo(dato)
        nodo.siguiente=self.primero
        self.primero=nodo

# Insertar al final
    def insertar_2(self,dato):
        nodo=Nodo(dato)
        if not self.primero:
            self.primero=nodo
            return
        actual=self.primero
        while actual.siguiente:
            actual=actual.siguiente
        actual.siguiente = nodo

# Eliminar


    def eliminar(self,dato):
        actual=self.primero
        anterior=None

        while actual and actual.dato != dato:
            anterior = actual
            actual = actual.siguiente

        if not actual:
            return  # no encontrado

        if not anterior:  # eliminar cabeza
            self.primero = actual.siguiente
        else:
            anterior.siguiente = actual.siguiente


    def mostrar (self):
        if self.primero is None:
            print("Lista vacia")
        actual=self.primero
        while actual:
            print(actual.dato, end="-->")
            actual=actual.siguiente
        print("None")


## Prueba ingresando al inicio un dato
lista1=LSL()
lista1.insertar(7)
lista1.mostrar()
lista1.insertar(10)
lista1.mostrar()
lista1.insertar(2)
lista1.mostrar()
lista1.eliminar(10)
lista1.mostrar() 

## Prueba ingresando un dato
lista2=LSL()
lista2.insertar_2(7)
lista2.mostrar()
lista2.insertar_2(10)
lista2.mostrar()
lista2.insertar_2(2)
lista2.mostrar()
lista2.eliminar(10)
lista2.mostrar() 