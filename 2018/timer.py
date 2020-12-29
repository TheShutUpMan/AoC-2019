import time


def timer(func):
    def func_wrapper():
        start = time.time()
        result = func()
        end = time.time()
        print('\n' + ''.join(['-'] * 26) + '\nFINISHED\nTIME:',
              end - start, '\n' + ''.join(['-'] * 26))

        return result

    return func_wrapper
