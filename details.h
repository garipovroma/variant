#pragma once

#include <array>
#include <functional>
#include <type_traits>

template <typename T>
struct variant_size;

struct in_place_t {
    explicit in_place_t() = default;
};
inline constexpr in_place_t in_place{};

template <typename T> struct in_place_type_t {
    explicit in_place_type_t() = default;
};
template <typename T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I> struct in_place_index_t {
    explicit in_place_index_t() = default;
};

template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

template <typename ...Alternatives>
struct variant;

inline constexpr std::size_t variant_npos = -1;

namespace details {

template <typename T>
struct variant_size_helper;

template <typename T>
concept DefaultConstructible = std::is_default_constructible_v<T>;

template <typename ...Ts>
concept AllCopyConstructible = (std::is_copy_constructible_v<Ts> && ...);

template <typename ...Ts>
concept AllTriviallyCopyConstructible = (std::is_trivially_copy_constructible_v<Ts>&&...);

template <typename ...Ts>
concept AllCopyAssignable = (std::is_copy_assignable_v<Ts> && ...);

template <typename ...Ts>
concept AllTriviallyCopyAssignable = (std::is_trivially_copy_assignable_v<Ts> && ...);

template <typename ...Ts>
concept AllTriviallyDestructible = (std::is_trivially_destructible_v<Ts> && ...);

template <typename ...Ts>
concept AllMoveConstructible = (std::is_move_constructible_v<Ts> && ...);

template <typename ...Ts>
concept AllMoveAssignable = (std::is_move_assignable_v<Ts> && ...);

template <typename ...Ts>
concept AllTriviallyMoveAssignable = (std::is_trivially_move_assignable_v<Ts> && ...);

template <typename ...Ts>
concept AllTriviallyMoveConstructible = (std::is_trivially_move_constructible_v<Ts>&&...);

template <typename ConvertType, typename Alternative, typename X = Alternative[]>
concept OverloadConstructorCond = requires(ConvertType&& a) {
    X{std::forward<ConvertType>(a)};
};

template <std::size_t index, typename ConvertType, typename ...Funs>
struct overloader_base;

template <std::size_t index, typename ConvertType, typename T>
struct overloader_base<index, ConvertType, T> {
    T operator()(T) requires(details::OverloadConstructorCond<ConvertType, T>){};
};

template <std::size_t index, typename ConvertType, typename Head, typename ...Tail>
struct overloader_base<index, ConvertType, Head, Tail...> : overloader_base<index + 1, ConvertType, Tail...> {
    using overloader_base<index + 1, ConvertType, Tail...>::operator();
    Head operator()(Head) requires(details::OverloadConstructorCond<ConvertType, Head>){};
};

template <typename ConvertType, typename ...Funs>
struct overloader : overloader_base<0, ConvertType, Funs...> {
    using overloader_base<0, ConvertType, Funs...>::operator();
};

template <typename T>
struct is_in_place_type_t : std::false_type {};

template <typename X>
struct is_in_place_type_t<in_place_type_t<X>> : std::true_type {};

template <typename T>
struct is_in_place_index_t : std::false_type {};

template <std::size_t X>
struct is_in_place_index_t<in_place_index_t<X>> : std::true_type {};

template <typename ConvertType, typename SelectedType, typename ...Alternatives>
concept ConvertingConditions = (sizeof...(Alternatives) != 0)
                            && std::is_convertible_v<ConvertType, SelectedType>
                            && !std::is_same_v<std::remove_cvref<ConvertType>, variant<Alternatives...>>
                            && !is_in_place_type_t<std::remove_cvref<ConvertType>>::value
                            && !is_in_place_index_t<std::remove_cvref<ConvertType>>::value;

template <typename T, std::size_t count, typename ...Ts>
struct count_t;

template <typename T, std::size_t count>
struct count_t<T, count> {
    static constexpr std::size_t value = count;
};

template <typename T, typename ...Ts>
inline constexpr std::size_t count_v = count_t<T, 0, Ts...>::value;

template <typename T, std::size_t count, typename Head, typename... Tail>
struct count_t<T, count, Head, Tail...> {
    static constexpr std::size_t value = std::is_same_v<T, Head> + count_v<T, Tail...>;
};

template <typename T, typename ...Ts>
concept ExactlyOnce = count_v<T, Ts...> == 1;

template <std::size_t ind, typename ...Ts>
struct get_by_ind;

namespace {
    struct empty_type {};
}

template <std::size_t ind>
struct get_by_ind<ind> {
    using type = empty_type;
};

template <typename Head, typename ...Tail>
struct get_by_ind<0, Head, Tail...> {
    using type = Head;
};

template <std::size_t ind, typename Head, typename ...Tail>
struct get_by_ind<ind, Head, Tail...> {
    using type = typename get_by_ind<ind - 1, Tail...>::type;
};

template <std::size_t ind, typename ...Ts>
using get_by_ind_t = typename get_by_ind<ind, Ts...>::type;

template <typename T, typename ...Ts>
struct variadic_find;

template <typename T>
struct variadic_find<T> : std::false_type {};

template <typename T, typename ...Tail>
struct variadic_find<T, T, Tail...> : std::true_type {};

template <typename T, typename Head, typename ...Tail>
struct variadic_find<T, Head, Tail...> : variadic_find<T, Tail...> {};

template <typename T, typename ...Ts>
inline constexpr bool variadic_find_t = variadic_find<T, Ts...>::value;

template <typename T, size_t ind, typename ...Ts>
struct get_ind;

template <typename T, size_t ind>
struct get_ind<T, ind> : std::integral_constant<std::ptrdiff_t, -1>{};

template <typename T, std::size_t ind, typename Head, typename ...Tail>
struct get_ind<T, ind, Head, Tail...> : get_ind<T, ind + 1, Tail...> {};

template <typename T, std::size_t ind, typename ...Tail>
struct get_ind<T, ind, T, Tail...> : std::integral_constant<std::size_t, ind> {};

template <typename T, typename ...Ts>
inline constexpr std::size_t get_ind_v = get_ind<T, 0, Ts...>::value;

struct empty_ctor_type {};

inline empty_ctor_type empty_ctor_v{};

}

namespace visitor {

template <bool ByType, typename Func, typename ...Variants, std::size_t ...Indexes>
constexpr auto get_invocation_tensor(std::index_sequence<Indexes...>) {
    if constexpr (ByType) {
        struct invoker {
            static constexpr decltype(auto) dispatch(Func f, Variants ...variants) {
                return (std::forward<Func>(f))(get<Indexes>(std::forward<Variants>(variants))...);
            }
        };
        return &invoker::dispatch;
    } else {
        struct invoker {
            static constexpr decltype(auto) dispatch(Func f, Variants ...variants) {
                return (std::forward<Func>(f))(in_place_index<Indexes>...);
            }
        };
        return &invoker::dispatch;
    }
}

template <bool ByType, typename Fun, typename ...Variants, std::size_t ...AccumulatedIndexes,
          std::size_t ...CurIndexes, typename ...Rest>
constexpr auto get_invocation_tensor(
    std::index_sequence<AccumulatedIndexes...>, std::index_sequence<CurIndexes...>, Rest ...rest) {
    return std::array{
        get_invocation_tensor<ByType, Fun, Variants...>
            (std::index_sequence<AccumulatedIndexes..., CurIndexes>(), rest...)...};
}

template <bool ByType, typename Func, typename ...Variants>
constexpr auto get_invocation_tensor() {
    return get_invocation_tensor<ByType, Func, Variants...>(
        std::index_sequence<>(),
            std::make_index_sequence<details::variant_size_helper<std::remove_reference_t<Variants>>::value>()...);
}

template <typename T>
constexpr T const& tensor_get(T& elem) { return elem; }

template <typename T, typename ...Indexes>
constexpr auto const& tensor_get(T& elems, std::size_t i, Indexes ...indexes) {
    return tensor_get(elems[i], indexes...);
}

template <typename T, typename ...Indexes>
constexpr auto const& tensor_get(T& elems, Indexes ...indexes) {
    return tensor_get(elems, indexes...);
}

template <typename Func, typename... Variants>
inline constexpr auto indexed_tensor_v = visitor::get_invocation_tensor<false, Func&&, Variants&&...>();

template <typename Func, typename... Variants>
constexpr decltype(auto) visit_by_index(Func&& f, Variants&& ... variants) {
    return visitor::tensor_get(indexed_tensor_v<Func, Variants...>, variants.index()...)(std::forward<Func>(f), std::forward<Variants>(variants)...);
}

template <typename Func, typename... Variants>
inline constexpr auto typed_tensor_v = visitor::get_invocation_tensor<true, Func&&, Variants&&...>();

}

struct bad_variant_access : std::exception {
    bad_variant_access() noexcept { }

    const char* what() const noexcept override {
        return bad_variant_str;
    }
  private:
    const char* bad_variant_str = "bad variant access";
};

template <typename Fun, typename ...Variants>
constexpr decltype(auto) visit(Fun&& f, Variants&& ...variants) {
    if ((variants.valueless_by_exception() || ...)) {
        throw bad_variant_access();
    }
    return visitor::tensor_get(visitor::typed_tensor_v<Fun, Variants...>, variants.index()...)(std::forward<Fun>(f), std::forward<Variants>(variants)...);
}

